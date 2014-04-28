{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
      find
    , findFilesSource
    , findFiles
    , findFilePaths
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
    , maxdepth_
    , mindepth_
    , ignoreReaddirRace_
    , noIgnoreReaddirRace_
    , amin_
    , atime_
    , anewer_
    , empty_
    , executable_
    , gid_
    , name_
    , getDepth
    , filename_
    , filenameS_
    , pathname_
    , pathnameS_
    , getFilePath
    , follow_
    , prune_

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

import           Conduit
import           Control.Applicative
import           Control.Exception
import           Control.Monad hiding (forM_)
import           Control.Monad.Morph
import           Control.Monad.State.Class
import           Data.Attoparsec.Text as A
import           Data.Bits
import qualified Data.Cond as Cond
import           Data.Cond hiding (test)
import           Data.Foldable hiding (elem, find)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text, unpack, pack)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Filesystem.Path.CurrentOS (FilePath, encodeString, filename)
import           Prelude hiding (FilePath)
import           System.Posix.Files
import           System.Posix.Types
import qualified Text.Regex.Posix as R ((=~))

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
    { findFollowSymlinks    :: Bool
    , findContentsFirst     :: Bool
    , findIgnoreReaddirRace :: Bool
    , findIgnoreResults     :: Bool
    }

defaultFindOptions :: FindOptions
defaultFindOptions = FindOptions
    { findFollowSymlinks    = True
    , findContentsFirst     = False
    , findIgnoreReaddirRace = False
    , findIgnoreResults     = False
    }

data FileEntry = FileEntry
    { entryPath        :: !FilePath
    , entryDepth       :: !Int
    , entryFindOptions :: !(FindOptions)
    , entryStatus      :: !(Maybe FileStatus)
      -- ^ This is Nothing until we determine stat should be called.
    }

newFileEntry :: FilePath -> Int -> FindOptions -> FileEntry
newFileEntry p d f = FileEntry p d f Nothing

instance Show FileEntry where
    show entry = "FileEntry "
              ++ show (entryPath entry)
              ++ " " ++ show (entryDepth entry)

getFilePath :: Monad m => CondT FileEntry m FilePath
getFilePath = gets entryPath

pathname_ :: Monad m => (FilePath -> Bool) -> CondT FileEntry m ()
pathname_ f = guard . f =<< getFilePath

pathnameS_ :: Monad m => (String -> Bool) -> CondT FileEntry m ()
pathnameS_ f = pathname_ (f . encodeString)

filename_ :: Monad m => (FilePath -> Bool) -> CondT FileEntry m ()
filename_ f = pathname_ (f . filename)

filenameS_ :: Monad m => (String -> Bool) -> CondT FileEntry m ()
filenameS_ f = pathname_ (f . encodeString . filename)

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

ignoreReaddirRace_ :: Monad m => CondT FileEntry m ()
ignoreReaddirRace_ =
    modifyFindOptions $ \opts -> opts { findIgnoreReaddirRace = True }

noIgnoreReaddirRace_ :: Monad m => CondT FileEntry m ()
noIgnoreReaddirRace_ =
    modifyFindOptions $ \opts -> opts { findIgnoreReaddirRace = False }

maxdepth_ :: Monad m => Int -> CondT FileEntry m ()
maxdepth_ l = getDepth >>= guard . (<= l)

mindepth_ :: Monad m => Int -> CondT FileEntry m ()
mindepth_ l = getDepth >>= guard . (>= l)

prune_ :: Monad m => CondT a m ()
prune_ = prune

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

anewer_ :: MonadIO m => FilePath -> CondT FileEntry m ()
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
    f = posixSecondsToUTCTime . accessTimeHiRes

-- cmin_ = error "NYI"
-- cnewer_ = error "NYI"
-- ctime_ = error "NYI"

empty_ :: MonadIO m => CondT FileEntry m ()
empty_ = (regular   >> hasStatus ((== 0) . fileSize))
     <|> (directory >> hasStatus ((== 2) . linkCount))

executable_ :: MonadIO m => CondT FileEntry m ()
executable_ = executable

gid_ :: MonadIO m => Int -> CondT FileEntry m ()
gid_ n = hasStatus ((== n) . fromIntegral . fileGroup)

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

name_ :: Monad m => FilePath -> CondT FileEntry m ()
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
        case ms of
            Just s  -> return $ Just (s, entry { entryStatus = Just s })
            Nothing -> return Nothing
  where
    follow = findFollowSymlinks . entryFindOptions
    doStat = (if fromMaybe (follow entry) mfollow
              then getFileStatus
              else getSymbolicLinkStatus) $ encodeString (entryPath entry)
    wrapStat = liftIO $ catch (Just <$> doStat) $ \e ->
        if findIgnoreReaddirRace opts
        then return Nothing
        else throwIO (e :: IOException)
      where
        opts = entryFindOptions entry

applyStat :: MonadIO m => Maybe Bool -> CondT FileEntry m FileStatus
applyStat mfollow = do
    e <- get
    ms <- lift (getStat mfollow e)
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
regular = hasStatus isRegularFile

executable :: MonadIO m => CondT FileEntry m ()
executable = hasMode ownerExecuteMode

directory :: MonadIO m => CondT FileEntry m ()
directory = hasStatus isDirectory

hasMode :: MonadIO m => FileMode -> CondT FileEntry m ()
hasMode m = hasStatus (\s -> fileMode s .&. m /= 0)

withStatusTime :: MonadIO m
               => (FileStatus -> POSIXTime) -> (UTCTime -> Bool)
               -> CondT FileEntry m ()
withStatusTime g f = hasStatus (f . posixSecondsToUTCTime . g)

lastAccessed_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastAccessed_ = withStatusTime accessTimeHiRes

lastModified_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastModified_ = withStatusTime modificationTimeHiRes

-- | This is a re-export of 'Text.Regex.Posix.=~', with the types changed for
--   use with this module.  For example, you can simply say:
--
-- @
--    filename_ (=~ \"\\\\.hs$\")
-- @
--
-- Which is the same thing as:
--
-- @
--    regex \"\\\\.hs$\"
-- @
(=~) :: FilePath -> Text -> Bool
str =~ pat = encodeString str R.=~ unpack pat

regex :: Monad m => Text -> CondT FileEntry m ()
regex pat = filename_ (=~ pat)

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: Monad m => CondT FileEntry m ()
ignoreVcs = when_ (filename_ (`elem` vcsDirs)) prune
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

-- | Find every entry whose filename part matching the given filename globbing
--   expression.  For example: @glob "*.hs"@.
glob :: Monad m => Text -> CondT FileEntry m ()
glob g = case parseOnly globParser g of
    Left e  -> error $ "Failed to parse glob: " ++ e
    Right x -> regex ("^" <> x <> "$")
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
            return . pack $ if x `elem` ".()^$"
                            then ['\\', x]
                            else [x]

-- | Find file entries in a directory tree, recursively, applying the given
--   recursion predicate to the search.  This conduit yields pairs of type
--   @(FileEntry, a)@, where is the return value from the predicate at each
--   step.
findFilesSource :: (MonadIO m, MonadResource m)
                => FindOptions
                -> FilePath
                -> CondT FileEntry m a
                -> Producer m (FileEntry, a)
findFilesSource opts startPath predicate =
    wrap $ go (newFileEntry startPath 0 opts) $ hoist lift predicate
  where
    wrap = mapInput (const ()) (const Nothing)

    go x pr = do
        ((mres, mcond), entry) <- applyCondT x pr
        let opts' = entryFindOptions entry
            this  = unless (findIgnoreResults opts') $ yieldEntry entry mres
            next  = walkChildren entry mcond
        if findContentsFirst opts'
            then next >> this
            else this >> next

    yieldEntry entry mres =
        -- If the item matched, also yield the predicate's result value.
        forM_ mres $ yield . (entry,)

    walkChildren entry@(FileEntry path depth opts' _) mcond =
        -- If the conditional matched, we are requested to recurse if this
        -- is a directory
        forM_ mcond $ \cond -> do
            -- If no status has been determined, we must do so now in order
            -- to know whether to actually recurse or not.
            descend <- fmap (isDirectory . fst) <$> getStat Nothing entry
            when (descend == Just True) $
                (sourceDirectory path =$) $ awaitForever $ \fp ->
                    wrap $ go (newFileEntry fp (succ depth) opts') cond

findFiles :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
          => FindOptions
          -> FilePath
          -> CondT FileEntry m a
          -> m ()
findFiles opts path predicate =
    runResourceT $ findFilesSource
        opts { findIgnoreResults = True } path (hoist lift predicate)
        $$ sinkNull

-- | A simpler version of 'findFiles', which yields only 'FilePath' values,
--   and ignores any values returned by the predicate action.
findFilePaths :: (MonadIO m, MonadResource m)
              => FindOptions
              -> FilePath
              -> CondT FileEntry m a
              -> Producer m FilePath
findFilePaths opts path predicate =
    findFilesSource opts path predicate =$= mapC (entryPath . fst)

-- | Calls 'findFilePaths' with the default set of finding options.
--   Equivalent to @findFilePaths defaultFindOptions@.
find :: (MonadIO m, MonadResource m)
     => FilePath -> CondT FileEntry m a -> Producer m FilePath
find = findFilePaths defaultFindOptions

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles'.
test :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
test matcher path =
    Cond.test (newFileEntry path 0 defaultFindOptions) matcher

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles', but do not follow symlinks.
ltest :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
ltest matcher path = Cond.test
    (newFileEntry path 0 defaultFindOptions { findFollowSymlinks = False })
    matcher
