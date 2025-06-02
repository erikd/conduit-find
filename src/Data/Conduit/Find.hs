{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Conduit.Find
    (
    -- * Introduction
    -- $intro

    -- ** Basic comparison with GNU find
    -- $gnufind

    -- ** Performance
    -- $performance

    -- ** Other notes
    -- $notes

    -- * Finding functions
      sourceFindFiles
    , find
    , findFiles
    , findFilePaths
    , FindOptions(..)
    , defaultFindOptions
    , test
    , ltest
    , stat
    , lstat
    , hasStatus

      -- * File path predicates
    , glob
    , regex
    , ignoreVcs

      -- * GNU find compatibility predicates
    , depth_
    , follow_
    , noleaf_
    , prune_
    , maxdepth_
    , mindepth_
    , ignoreErrors_
    , noIgnoreErrors_
    , amin_
    , atime_
    , anewer_
    , empty_
    , executable_
    , gid_
    , name_
    , getDepth
    , filename_
    , pathname_
    , getFilePath

    -- * File entry predicates (uses stat information)
    , regular
    , directory
    , hasMode
    , executable
    , lastAccessed_
    , lastModified_

    -- * Predicate combinators
    , module Cond
    , (=~)

    -- * Types and type classes
    , FileEntry(..)
    ) where

import           Control.Applicative (Alternative (..))
import           Control.Exception (IOException, catch, throwIO)
import           Control.Monad hiding (forM_, forM)
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Morph (hoist, lift)
import           Control.Monad.State.Class (get, gets, modify, put)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadResource)

import           Data.Attoparsec.Text as A
import           Data.Bits ((.&.))
import           Data.Conduit (ConduitT, runConduitRes, (.|))
import qualified Data.Conduit as DC
import qualified Data.Conduit.List as DCL
import qualified Data.Cond as Cond
import           Data.Cond hiding (test)
import qualified Data.Conduit.Filesystem as CF
#if LEAFOPT
import           Data.IORef (IORef, newIORef, modifyIORef, readIORef)
#endif
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Text (Text, unpack, pack)
import           Data.Time (UTCTime,  diffUTCTime)
import           Data.Time.Clock.POSIX (getCurrentTime, posixSecondsToUTCTime)
import qualified System.FilePath as FP
import           System.PosixCompat.Files (FileStatus, linkCount)
import qualified System.PosixCompat.Files as Files
import           System.PosixCompat.Types (EpochTime, FileMode)
import           Text.Regex.Posix ((=~))

{- $intro

**find-conduit** is essentially a souped version of GNU find for Haskell,
using a DSL to provide both ease of us, and extensive flexbility.

In its simplest form, let's compare some uses of find to find-conduit.  Bear
in mind that the result of the find function is a conduit, so you're expected
to either sink it to a list, or operate on the file paths as they are yielded.
-}

{- $gnufind

A typical find command:

@
find src -name '*.hs' -type f -print
@

Would in find-conduit be:

@
find "src" (glob \"*.hs\" \<\> regular) $$ mapM_C (liftIO . print)
@

The 'glob' predicate matches the file basename against the globbing pattern,
while the 'regular' predicate matches plain files.

A more complicated example:

@
find . -size +100M -perm 644 -mtime 1
@

Now in find-conduit:

@
let megs = 1024 * 1024
    days = 86400
now <- liftIO getCurrentTime
find \".\" ( fileSize (> 100*megs)
        \<\> hasMode 0o644
        \<\> lastModified (> addUTCTime now (-(1*days)))
         )
@

Appending predicates like this expressing an "and" relationship.  Use '<|>' to
express "or".  You can also negate any predicate:

@
find \".\" (not_ (hasMode 0o644))
@

By default, predicates, whether matching or not, will allow recursion into
directories.  In order to express that matching predicate should disallow
recursion, use 'prune':

@
find \".\" (prune (depth (> 2)))
@

This is the same as using '-maxdepth 2' in find.

@
find \".\" (prune (filename_ (== \"dist\")))
@

This is the same as:

@
find . \\( -name dist -prune \\) -o -print
@
-}

{- $performance

find-conduit strives to make file-finding a well performing operation.  To
this end, a composed Predicate will only call stat once per entry being
considered; and if you prune a directory, it is not traversed at all.

By default, 'find' calls stat for every file before it applies the predicate,
in order to ensure that only one such call is needed.  Sometimes, however, you
know just from the FilePath that you don't want to consider a certain file, or
you want to prune a directory tree.

To support these types of optimized queries, a variant of find is provided
called 'findWithPreFilter'.  This takes two predicates: one that is applied to
only the FilePath, before stat (or lstat) is called; and one that is applied
to the full file information after the stat.
-}

{- $notes

See 'Data.Cond' for more details on the Monad used to build predicates.
-}

data FindOptions = FindOptions
    { findFollowSymlinks   :: !Bool
    , findContentsFirst    :: !Bool
    , findIgnoreErrors     :: !Bool
    , findIgnoreResults    :: !Bool
    , findLeafOptimization :: !Bool
    }

defaultFindOptions :: FindOptions
defaultFindOptions = FindOptions
    { findFollowSymlinks   = True
    , findContentsFirst    = False
    , findIgnoreErrors     = False
    , findIgnoreResults    = False
    , findLeafOptimization = True
    }

data FileEntry = FileEntry
    { entryPath        :: !FP.FilePath
    , entryDepth       :: !Int
    , entryFindOptions :: !FindOptions
    , entryStatus      :: !(Maybe FileStatus)
      -- ^ This is Nothing until we determine stat should be called.
    }

newFileEntry :: FP.FilePath -> Int -> FindOptions -> FileEntry
newFileEntry fp d f = FileEntry fp d f Nothing

instance Show FileEntry where
    show entry = "FileEntry "
              ++ show (entryPath entry)
              ++ " " ++ show (entryDepth entry)

getFilePath :: Monad m => CondT FileEntry m FP.FilePath
getFilePath = gets entryPath

pathname_ :: Monad m => (FP.FilePath -> Bool) -> CondT FileEntry m ()
pathname_ f = guard . f =<< getFilePath

filename_ :: Monad m => (FP.FilePath -> Bool) -> CondT FileEntry m ()
filename_ f = pathname_ (f . FP.takeFileName)

getDepth :: Monad m => CondT FileEntry m Int
getDepth = gets entryDepth

modifyFindOptions :: Monad m
                  => (FindOptions -> FindOptions)
                  -> CondT FileEntry m ()
modifyFindOptions f =
    modify $ \e -> e { entryFindOptions = f (entryFindOptions e) }

------------------------------------------------------------------------
-- Workalike options for emulating GNU find.
------------------------------------------------------------------------

depth_ :: Monad m => CondT FileEntry m ()
depth_ = modifyFindOptions $ \opts -> opts { findContentsFirst = True }

follow_ :: Monad m => CondT FileEntry m ()
follow_ = modifyFindOptions $ \opts -> opts { findFollowSymlinks = True }

noleaf_ :: Monad m => CondT FileEntry m ()
noleaf_ = modifyFindOptions $ \opts -> opts { findLeafOptimization = False }

prune_ :: Monad m => CondT a m ()
prune_ = prune

ignoreErrors_ :: Monad m => CondT FileEntry m ()
ignoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = True }

noIgnoreErrors_ :: Monad m => CondT FileEntry m ()
noIgnoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = False }

maxdepth_ :: Monad m => Int -> CondT FileEntry m ()
maxdepth_ l = getDepth >>= guard . (<= l)

mindepth_ :: Monad m => Int -> CondT FileEntry m ()
mindepth_ l = getDepth >>= guard . (>= l)

-- xdev_ = error "NYI"

timeComp :: MonadIO m
         => ((UTCTime -> Bool) -> CondT FileEntry m ()) -> Int
         -> CondT FileEntry m ()
timeComp f n = do
    now <- liftIO getCurrentTime
    f (\t -> diffUTCTime now t > fromIntegral n)

amin_ :: MonadIO m => Int -> CondT FileEntry m ()
amin_ n = timeComp lastAccessed_ (n * 60)

atime_ :: MonadIO m => Int -> CondT FileEntry m ()
atime_ n = timeComp lastAccessed_ (n * 24 * 3600)

anewer_ :: MonadIO m => FP.FilePath -> CondT FileEntry m ()
anewer_ path = do
    e  <- get
    es <- applyStat Nothing
    ms <- getStat Nothing e { entryPath   = path
                            , entryStatus = Nothing
                            }
    case ms of
        Nothing     -> prune >> error "This is never reached"
        Just (s, _) -> guard $ diffUTCTime (f s) (f es) > 0
  where
    f = posixSecondsToUTCTime . realToFrac . Files.accessTime

-- cmin_ = error "NYI"
-- cnewer_ = error "NYI"
-- ctime_ = error "NYI"

empty_ :: MonadIO m => CondT FileEntry m ()
empty_ = (regular   >> hasStatus ((== 0) . Files.fileSize))
     <|> (directory >> hasStatus ((== 2) . linkCount))

executable_ :: MonadIO m => CondT FileEntry m ()
executable_ = executable

gid_ :: MonadIO m => Int -> CondT FileEntry m ()
gid_ n = hasStatus ((== n) . fromIntegral . Files.fileGroup)

{-
group_ name
ilname_ pat
iname_ pat
inum_ n
ipath_ pat
iregex_ pat
iwholename_ pat
links_ n
lname_ pat
mmin_
mtime_
-}

name_ :: Monad m => FP.FilePath -> CondT FileEntry m ()
name_ = filename_ . (==)

{-
newer_ path
newerXY_ ref
nogroup_
nouser_
path_ pat
perm_ mode :: Perm
readable_
regex_ pat
samefile_ path
size_ n :: Size
type_ c
uid_ n
used_ n
user_ name
wholename_ pat
writable_
xtype_ c
-}

------------------------------------------------------------------------

statFilePath :: Bool -> Bool -> FP.FilePath -> IO (Maybe FileStatus)
statFilePath follow ignoreErrors path = do
    let doStat = (if follow
                  then Files.getFileStatus
                  else Files.getSymbolicLinkStatus) path
    catch (Just <$> doStat) $ \e ->
        if ignoreErrors
        then return Nothing
        else throwIO (e :: IOException)

-- | Get the current status for the file.  If the status being requested is
--   already cached in the entry information, simply return it from there.
getStat :: MonadIO m
        => Maybe Bool
        -> FileEntry
        -> m (Maybe (FileStatus, FileEntry))
getStat mfollow entry = case entryStatus entry of
    Just s
        | maybe True (== follow entry) mfollow ->
            return $ Just (s, entry)
        | otherwise -> fmap (, entry) `liftM` wrapStat
    Nothing -> do
        ms <- wrapStat
        return $ case ms of
            Just s  -> Just (s, entry { entryStatus = Just s })
            Nothing -> Nothing
  where
    follow   = findFollowSymlinks . entryFindOptions
    wrapStat = liftIO $ statFilePath
        (fromMaybe (findFollowSymlinks opts) mfollow)
        (findIgnoreErrors opts)
        (entryPath entry)
      where
        opts = entryFindOptions entry

applyStat :: MonadIO m => Maybe Bool -> CondT FileEntry m FileStatus
applyStat mfollow = do
    ms <- lift . getStat mfollow =<< get
    case ms of
        Nothing      -> prune >> error "This is never reached"
        Just (s, e') -> s <$ put e'

lstat :: MonadIO m => CondT FileEntry m FileStatus
lstat = applyStat (Just False)

stat :: MonadIO m => CondT FileEntry m FileStatus
stat = applyStat (Just True)

hasStatus :: MonadIO m => (FileStatus -> Bool) -> CondT FileEntry m ()
hasStatus f = guard . f =<< applyStat Nothing

regular :: MonadIO m => CondT FileEntry m ()
regular = hasStatus Files.isRegularFile

executable :: MonadIO m => CondT FileEntry m ()
executable = hasMode Files.ownerExecuteMode

directory :: MonadIO m => CondT FileEntry m ()
directory = hasStatus Files.isDirectory

hasMode :: MonadIO m => FileMode -> CondT FileEntry m ()
hasMode m = hasStatus (\s -> Files.fileMode s .&. m /= 0)

withStatusTime :: MonadIO m
               => (FileStatus -> EpochTime) -> (UTCTime -> Bool)
               -> CondT FileEntry m ()
withStatusTime g f = hasStatus (f . posixSecondsToUTCTime . realToFrac . g)

lastAccessed_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastAccessed_ = withStatusTime Files.accessTime

lastModified_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastModified_ = withStatusTime Files.modificationTime

regex :: Monad m => String -> CondT FileEntry m ()
regex pat = filename_ (=~ pat)

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: Monad m => CondT FileEntry m ()
ignoreVcs = when_ (filename_ (`elem` vcsDirs)) prune
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

-- | Find every entry whose filename part matching the given filename globbing
--   expression.  For example: @glob "*.hs"@.
glob :: Monad m => String -> CondT FileEntry m ()
glob g = case parseOnly globParser (pack g) of
    Left e  -> error $ "Failed to parse glob: " ++ e
    Right x -> regex ("^" <> unpack x <> "$")
  where
    globParser :: Parser Text
    globParser = fmap mconcat $ many $
            char '*' *> return ".*"
        <|> char '?' *> return "."
        <|> string "[]]" *> return "[]]"
        <|> (\x y z -> pack ((x:y) ++ [z]))
                <$> char '['
                <*> manyTill anyChar (A.try (char ']'))
                <*> char ']'
        <|> do
            x <- anyChar
            return . pack $ if x `elem` (".()^$" :: String)
                            then ['\\', x]
                            else [x]

#if LEAFOPT
type DirCounter = IORef Word

newDirCounter :: MonadIO m => m DirCounter
newDirCounter = liftIO $ newIORef 1
#else
type DirCounter = ()

newDirCounter :: MonadIO m => m DirCounter
newDirCounter = return ()
#endif

-- | Find file entries in a directory tree, recursively, applying the given
--   recursion predicate to the search.  This conduit yields pairs of type
--   @(FileEntry, a)@, where is the return value from the predicate at each
--   step.
sourceFindFiles :: MonadResource m
                => FindOptions
                -> FilePath
                -> CondT FileEntry m a
                -> ConduitT i (FileEntry, a) m ()
sourceFindFiles findOptions startPath predicate = do
    startDc <- newDirCounter
    walk startDc
        (newFileEntry startPath 0 findOptions)
        startPath
        predicate
  where
    walk :: MonadResource m
         => DirCounter
         -> FileEntry
         -> FP.FilePath
         -> CondT FileEntry m a
         -> ConduitT i (FileEntry, a) m ()
    walk !dc !entry !path !cond = do
        ((!mres, !mcond), !entry') <- lift $ applyCondT entry cond
        let opts' = entryFindOptions entry
            this  = unless (findIgnoreResults opts') $
                        yieldEntry entry' mres
            next  = walkChildren dc entry' path mcond
        if findContentsFirst opts'
            then next >> this
            else this >> next
      where
        yieldEntry _      Nothing    = return ()
        yieldEntry entry' (Just res) = DC.yield (entry', res)

    walkChildren :: MonadResource m
                 => DirCounter
                 -> FileEntry
                 -> FP.FilePath
                 -> Maybe (CondT FileEntry m a)
                 -> ConduitT i (FileEntry, a) m ()
    walkChildren _ _ _ Nothing = return ()
    -- If the conditional matched, we are requested to recurse if this is a
    -- directory
    walkChildren !dc !entry !path (Just !cond) = do
        st <- lift $ checkIfDirectory dc entry path
        when (fmap Files.isDirectory st == Just True) $ do
#if LEAFOPT
            -- Update directory count for the parent directory.
            liftIO $ modifyIORef dc pred
            -- Track the directory count for this child path.
            let leafOpt = findLeafOptimization (entryFindOptions entry)
            let lc = linkCount (fromJust st) - 2
                opts' = (entryFindOptions entry)
                    { findLeafOptimization = leafOpt && lc >= 0
                    }
            dc' <- liftIO $ newIORef (fromIntegral lc :: Word)
#else
            let dc'   = dc
                opts' = entryFindOptions entry
#endif
            CF.sourceDirectory path .| DC.awaitForever (go dc' opts')
      where
        go dc' opts' fp =
            let entry' = newFileEntry fp (succ (entryDepth entry)) opts'
            in walk dc' entry' fp cond

    -- Return True if the given entry is a directory.  We can sometimes use
    -- "leaf optimization" on Linux to answer this question without performing
    -- a stat call.  This is possible because the link count of a directory is
    -- two more than the number of sub-directories it contains, so we've seen
    -- that many sub-directories, the remaining entries must be files.
    checkIfDirectory :: MonadResource m
                     => DirCounter
                     -> FileEntry
                     -> FP.FilePath
                     -> m (Maybe FileStatus)
    checkIfDirectory !dc !entry !path = do
#if LEAFOPT
        let leafOpt = findLeafOptimization (entryFindOptions entry)
        doStat <- if leafOpt
                  then (> 0) <$> liftIO (readIORef dc)
                  else return True
#else
        let doStat = dc == () -- to quiet hlint warnings
#endif
        let opts = entryFindOptions entry
        if doStat
            then liftIO $ statFilePath
                (findFollowSymlinks opts)
                (findIgnoreErrors opts)
                path
            else return Nothing

findFiles :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnliftIO m)
          => FindOptions
          -> FilePath
          -> CondT FileEntry m a
          -> m ()
findFiles opts path predicate =
    runConduitRes $
        sourceFindFiles opts { findIgnoreResults = True } path
            (hoist lift predicate) .| DCL.sinkNull

-- | A simpler version of 'findFiles', which yields only 'FilePath' values,
--   and ignores any values returned by the predicate action.
findFilePaths :: (MonadIO m, MonadResource m)
              => FindOptions
              -> FilePath
              -> CondT FileEntry m a
              -> ConduitT i FilePath m ()
findFilePaths opts path predicate =
    sourceFindFiles opts path predicate .| DCL.map (entryPath . fst)

-- | Calls 'findFilePaths' with the default set of finding options.
--   Equivalent to @findFilePaths defaultFindOptions@.
find :: MonadResource m
     => FilePath -> CondT FileEntry m a -> DC.ConduitT i FilePath m ()
find = findFilePaths defaultFindOptions

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles'.
test :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
test matcher path =
    Cond.test (newFileEntry path 0 defaultFindOptions) matcher

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles', but do not follow symlinks.
ltest :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
ltest matcher path =
    Cond.test
        (newFileEntry path 0 defaultFindOptions
            { findFollowSymlinks = False })
        matcher
