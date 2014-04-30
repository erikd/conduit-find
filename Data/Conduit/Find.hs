{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
      sourceFindFiles
    -- , find
    , findFiles
    -- , findFilePaths
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

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.State.Class
import           Data.Attoparsec.Text as A
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Char (ord)
import qualified Data.Cond as Cond
import           Data.Cond hiding (test)
import           Data.Conduit.Find.Directory
import           Data.Conduit.Find.EParIO
import           Data.Conduit.Find.Types
import           Data.Monoid
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Clock.POSIX
import           Filesystem.Path.CurrentOS (FilePath, toText)
import           Prelude hiding (FilePath)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           System.PosixCompat.Files
import           System.PosixCompat.Types
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

getFilePath :: Monad m => CondT FileEntry m RawFilePath
getFilePath = gets entryPath
{-# INLINE getFilePath #-}

pathname_ :: Monad m => (RawFilePath -> Bool) -> CondT FileEntry m ()
pathname_ f = guard . f =<< getFilePath
{-# INLINE pathname_ #-}

filename_ :: Monad m => (RawFilePath -> Bool) -> CondT FileEntry m ()
filename_ f = pathname_ (f . takeFileName)
{-# INLINE filename_ #-}

getDepth :: Monad m => CondT FileEntry m Int
getDepth = gets entryDepth
{-# INLINE getDepth #-}

modifyFindOptions :: Monad m
                  => (FindOptions -> FindOptions) -> CondT FileEntry m ()
modifyFindOptions f =
    modify $ \e -> e { entryFindOptions = f (entryFindOptions e) }
{-# INLINE modifyFindOptions #-}

------------------------------------------------------------------------
-- Workalike options for emulating GNU find.
------------------------------------------------------------------------

depth_ :: Monad m => CondT FileEntry m ()
depth_ = modifyFindOptions $ \opts -> opts { findContentsFirst = True }
{-# INLINE depth_ #-}

follow_ :: Monad m => CondT FileEntry m ()
follow_ = modifyFindOptions $ \opts -> opts { findFollowSymlinks = True }
{-# INLINE follow_ #-}

noleaf_ :: Monad m => CondT FileEntry m ()
noleaf_ = modifyFindOptions $ \opts -> opts { findLeafOptimization = False }
{-# INLINE noleaf_ #-}

prune_ :: Monad m => CondT a m ()
prune_ = prune
{-# INLINE prune_ #-}

ignoreErrors_ :: Monad m => CondT FileEntry m ()
ignoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = True }
{-# INLINE ignoreErrors_ #-}

noIgnoreErrors_ :: Monad m => CondT FileEntry m ()
noIgnoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = False }
{-# INLINE noIgnoreErrors_ #-}

maxdepth_ :: Monad m => Int -> CondT FileEntry m ()
maxdepth_ l = getDepth >>= guard . (<= l)
{-# INLINE maxdepth_ #-}

mindepth_ :: Monad m => Int -> CondT FileEntry m ()
mindepth_ l = getDepth >>= guard . (>= l)
{-# INLINE mindepth_ #-}

-- xdev_ = error "NYI"

timeComp :: MonadIO m
         => ((UTCTime -> Bool) -> CondT FileEntry m ()) -> Int
         -> CondT FileEntry m ()
timeComp f n = do
    now <- liftIO getCurrentTime
    f (\t -> diffUTCTime now t > fromIntegral n)
{-# INLINE timeComp #-}

amin_ :: MonadIO m => Int -> CondT FileEntry m ()
amin_ n = timeComp lastAccessed_ (n * 60)
{-# INLINE amin_ #-}

atime_ :: MonadIO m => Int -> CondT FileEntry m ()
atime_ n = timeComp lastAccessed_ (n * 24 * 3600)
{-# INLINE atime_ #-}

anewer_ :: MonadIO m => RawFilePath -> CondT FileEntry m ()
anewer_ path = do
    e  <- get
    es <- applyStat Nothing
    ms <- liftIO $ getStat Nothing
        e { entryPath   = path
          , entryStatus = Nothing
          }
    case ms of
        Nothing     -> prune >> error "This is never reached"
        Just (s, _) -> guard $ diffUTCTime (f s) (f es) > 0
  where
    f = posixSecondsToUTCTime . realToFrac . accessTime

-- cmin_ = error "NYI"
-- cnewer_ = error "NYI"
-- ctime_ = error "NYI"

empty_ :: MonadIO m => CondT FileEntry m ()
empty_ = (regular   >> hasStatus ((== 0) . fileSize))
     <|> (directory >> hasStatus ((== 2) . linkCount))
{-# INLINE empty_ #-}

executable_ :: MonadIO m => CondT FileEntry m ()
executable_ = executable
{-# INLINE executable_ #-}

gid_ :: MonadIO m => Int -> CondT FileEntry m ()
gid_ n = hasStatus ((== n) . fromIntegral . fileGroup)
{-# INLINE gid_ #-}

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

name_ :: Monad m => RawFilePath -> CondT FileEntry m ()
name_ = filename_ . (==)
{-# INLINE name_ #-}

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

applyStat :: MonadIO m => Maybe Bool -> CondT FileEntry m FileStatus
applyStat mfollow = do
    ms <- liftIO . getStat mfollow =<< get
    case ms of
        Nothing      -> prune >> error "This is never reached"
        Just (s, e') -> s <$ put e'
{-# INLINE applyStat #-}

lstat :: MonadIO m => CondT FileEntry m FileStatus
lstat = applyStat (Just False)
{-# INLINE lstat #-}

stat :: MonadIO m => CondT FileEntry m FileStatus
stat = applyStat (Just True)
{-# INLINE stat #-}

hasStatus :: MonadIO m => (FileStatus -> Bool) -> CondT FileEntry m ()
hasStatus f = guard . f =<< applyStat Nothing
{-# INLINE hasStatus #-}

regular :: MonadIO m => CondT FileEntry m ()
regular = hasStatus isRegularFile
{-# INLINE regular #-}

executable :: MonadIO m => CondT FileEntry m ()
executable = hasMode ownerExecuteMode
{-# INLINE executable #-}

directory :: MonadIO m => CondT FileEntry m ()
directory = hasStatus isDirectory
{-# INLINE directory #-}

hasMode :: MonadIO m => FileMode -> CondT FileEntry m ()
hasMode m = hasStatus (\s -> fileMode s .&. m /= 0)
{-# INLINE hasMode #-}

withStatusTime :: MonadIO m
               => (FileStatus -> EpochTime) -> (UTCTime -> Bool)
               -> CondT FileEntry m ()
withStatusTime g f = hasStatus (f . posixSecondsToUTCTime . realToFrac . g)
{-# INLINE withStatusTime #-}

lastAccessed_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastAccessed_ = withStatusTime accessTime
{-# INLINE lastAccessed_ #-}

lastModified_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastModified_ = withStatusTime modificationTime
{-# INLINE lastModified_ #-}

regex :: Monad m => String -> CondT FileEntry m ()
regex pat = filename_ (=~ pat)
{-# INLINE regex #-}

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: Monad m => CondT FileEntry m ()
ignoreVcs = when_ (filename_ (`elem` vcsDirs)) prune
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]
{-# INLINE ignoreVcs #-}

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
            return . pack $ if x `elem` ".()^$"
                            then ['\\', x]
                            else [x]

-- | Find file entries in a directory tree, recursively, applying the given
--   recursion predicate to the search.  This conduit yields pairs of type
--   @(FileEntry, a)@, where is the return value from the predicate at each
--   step.
sourceFindFiles :: (MonadIO m, MonadResource m)
                => FindOptions
                -> RawFilePath
                -> CondT FileEntry m a
                -> Producer m (FileEntry, a)
sourceFindFiles findOptions startPath =
    walkChildren (newFileEntry startPath 0 findOptions) (-1)
  where
    sep = fromIntegral (ord '/')

    walkChildren :: MonadResource m
                 => FileEntry
                 -> Int
                 -> CondT FileEntry m a
                 -> Producer m (FileEntry, a)
    -- If the conditional matched, we are requested to recurse if this is a
    -- directory
    walkChildren !entry !links !cond = do
        let path      = B.snoc (entryPath entry) sep
            opts      = entryFindOptions entry
            nextDepth = succ (entryDepth entry)

        -- jww (2014-04-30): Instead of True here, we need to determine if the
        -- filesystem supports the dirent->dt_type field.
        fps <- liftIO $ getDirectoryContentsAndAttrs path links True
        forM_ fps $ \(fp, misDir) -> do
            let childPath = B.append path fp
                child     = newFileEntry childPath nextDepth opts

            ((!mres, !mcond), !child') <- lift $ applyCondT child cond

            let opts' = entryFindOptions child'
                this = case mres of
                    Nothing -> return ()
                    Just res
                        | findIgnoreResults opts' -> return ()
                        | otherwise -> yield (child', res)
                next = handleTree child' misDir mcond

            if findContentsFirst opts'
                then next >> this
                else this >> next

    handleTree :: MonadResource m
               => FileEntry
               -> Maybe Bool
               -> Maybe (CondT FileEntry m a)
               -> Producer m (FileEntry, a)
    handleTree _ _ Nothing      = return ()
    handleTree _ (Just False) _ = return ()
#if !LEAFOPT
    -- If we cannot perform leaf optimization, there is no benefit to doing
    -- another stat just to read the link count, since we already know this
    -- is a directory.
    handleTree entry (Just True) (Just cond) =
        walkChildren entry (-1) cond
#endif
    handleTree entry Nothing (Just cond) = do
        (isDir, links, entry') <- do
            mres <- liftIO $ getStat Nothing entry
            case mres of
                Nothing -> return (False, -1, entry)
                Just (st, entry') ->
                    return ( isDirectory st
                           , fromIntegral (linkCount st)
                           , entry'
                           )
        when isDir $
            walkChildren entry' (if links < 2 then -1 else links) cond
    {-# INLINE handleTree #-}

convertPath :: FilePath -> RawFilePath
convertPath fp = either encodeUtf8 encodeUtf8 (toText fp)
{-# INLINE convertPath #-}

findFiles :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
          => FindOptions
          -> FilePath
          -> CondT FileEntry m a
          -> m ()
findFiles opts path predicate =
    runResourceT $
        sourceFindFiles opts { findIgnoreResults = True } (convertPath path)
            (hoist lift predicate) $$ sinkNull
{-# INLINE findFiles #-}

-- -- | A simpler version of 'findFiles', which yields only 'FilePath' values,
-- --   and ignores any values returned by the predicate action.
-- findFilePaths :: (MonadIO m, MonadResource m)
--               => FindOptions
--               -> FilePath
--               -> CondT FileEntry m a
--               -> Producer m FilePath
-- findFilePaths opts path predicate =
--     mapOutput decodeString $
--         sourceFindFiles opts path predicate =$= mapC (entryPath . fst)

-- -- | Calls 'findFilePaths' with the default set of finding options.
-- --   Equivalent to @findFilePaths defaultFindOptions@.
-- find :: (MonadIO m, MonadResource m)
--      => FilePath -> CondT FileEntry m a -> Producer m FilePath
-- find = findFilePaths defaultFindOptions

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles'.
test :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
test matcher path =
    Cond.test (newFileEntry (convertPath path) 0 defaultFindOptions) matcher
{-# INLINE test #-}

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles', but do not follow symlinks.
ltest :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
ltest matcher path =
    Cond.test
        (newFileEntry (convertPath path) 0 defaultFindOptions
            { findFollowSymlinks = False })
        matcher
{-# INLINE ltest #-}
