{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , depth_
    , name_
    , filename_
    , filenameS_
    , pathname_
    , pathnameS_
    , getFilePath
    , ignoreVcs

    -- * File entry predicates (uses stat information)
    , regular
    , directory
    , hasMode
    , executable
    , lastAccessed
    , lastModified

    -- * Predicate combinators
    , module Cond
    , (=~)

    -- * Types and type classes
    , FileEntry(..)
    ) where

import           Conduit
import           Control.Applicative
import           Control.Monad hiding (forM_)
import           Control.Monad.Morph
import           Control.Monad.State.Class
import           Data.Attoparsec.Text
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
    { findFollowSymlinks :: Bool
    , findContentsFirst  :: Bool
    }

defaultFindOptions :: FindOptions
defaultFindOptions = FindOptions
    { findFollowSymlinks = True
    , findContentsFirst  = False
    }

data FileEntry = FileEntry
    { entryPath        :: !FilePath
    , entryDepth       :: !Int
    , entryFindOptions :: !FindOptions
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
getFilePath = entryPath <$> get

pathname_ :: Monad m => (FilePath -> Bool) -> CondT FileEntry m ()
pathname_ f = guard . f =<< getFilePath

pathnameS_ :: Monad m => (String -> Bool) -> CondT FileEntry m ()
pathnameS_ f = pathname_ (f . encodeString)

filename_ :: Monad m => (FilePath -> Bool) -> CondT FileEntry m ()
filename_ f = pathname_ (f . filename)

filenameS_ :: Monad m => (String -> Bool) -> CondT FileEntry m ()
filenameS_ f = pathname_ (f . encodeString . filename)

name_ :: Monad m => FilePath -> CondT FileEntry m ()
name_ = filename_ . (==)

depth_ :: Monad m => (Int -> Bool) -> CondT FileEntry m ()
depth_ f = guard_ (f . entryDepth)

-- | Get the current status for the file.  If the status being requested is
--   already cached in the entry information, simply return it from there.
getStat :: MonadIO m => Maybe Bool -> FileEntry -> m (FileStatus, FileEntry)
getStat mfollow entry = case entryStatus entry of
    Just s
        | maybe True (== follow entry) mfollow ->
            return (s, entry)
        | otherwise -> doStat >>= \s' -> return (s', entry)
    Nothing -> do
        s <- doStat
        return (s, entry { entryStatus = Just s })
  where
    follow = findFollowSymlinks . entryFindOptions
    doStat = liftIO $ (if fromMaybe (follow entry) mfollow
                       then getFileStatus
                       else getSymbolicLinkStatus)
                    $ encodeString (entryPath entry)

applyStat :: MonadIO m => Maybe Bool -> CondT FileEntry m FileStatus
applyStat mfollow =
    get >>= \e -> lift (getStat mfollow e) >>= \(s, e') -> s <$ put e'

lstat :: MonadIO m => CondT FileEntry m FileStatus
lstat = applyStat (Just False)

stat :: MonadIO m => CondT FileEntry m FileStatus
stat = applyStat (Just True)

hasStatus :: MonadIO m => (FileStatus -> Bool) -> CondT FileEntry m ()
hasStatus f = guard . f =<< applyStat Nothing

regular :: MonadIO m => CondT FileEntry m ()
regular = hasStatus isRegularFile

directory :: MonadIO m => CondT FileEntry m ()
directory = hasStatus isDirectory

hasMode :: MonadIO m => FileMode -> CondT FileEntry m ()
hasMode m = hasStatus (\s -> fileMode s .&. m /= 0)

executable :: MonadIO m => CondT FileEntry m ()
executable = hasMode ownerExecuteMode

withStatusTime :: MonadIO m
               => (UTCTime -> Bool) -> (FileStatus -> POSIXTime)
               -> CondT FileEntry m ()
withStatusTime f g = hasStatus (f . posixSecondsToUTCTime . g)

lastAccessed :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastAccessed = flip withStatusTime accessTimeHiRes

lastModified :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastModified = flip withStatusTime modificationTimeHiRes

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
ignoreVcs = when_ (filename_ (`elem` vcsDirs)) reject
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
                <*> manyTill anyChar (try (char ']'))
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
findFiles :: (MonadIO m, MonadResource m)
          => FindOptions
          -> FilePath
          -> CondT FileEntry m a
          -> Source m (FileEntry, a)
findFiles opts startPath =
    go (newFileEntry startPath 0 opts) . hoist lift
  where
    go x pr = applyCondT x pr $ \e@(FileEntry path depth opts' _) mb mcond -> do
        -- If the item matched, also yield the predicate's result value.
        forM_ mb $ yield . (e,)

        -- If the conditional matched, we are requested to recurse if this
        -- is a directory
        forM_ mcond $ \cond -> do
            -- If no status has been determined, we must do so now in order
            -- to know whether to actually recurse or not.
            descend <- isDirectory . fst <$> getStat Nothing e
            when descend $
                (sourceDirectory path =$) $ awaitForever $ \fp ->
                    mapInput (const ()) (const Nothing) $
                        go (newFileEntry fp (succ depth) opts') cond

-- | A simpler version of 'findFiles', which yields only 'FilePath' values,
--   and ignores any values returned by the predicate action.
findFilePaths :: (MonadIO m, MonadResource m)
              => FindOptions
              -> FilePath
              -> CondT FileEntry m a
              -> Source m FilePath
findFilePaths opts path pr = findFiles opts path pr =$= mapC (entryPath . fst)

-- | Calls 'findFilePaths' with the default set of finding options.
--   Equivalent to @findFilePaths defaultFindOptions@.
find :: (MonadIO m, MonadResource m)
     => FilePath -> CondT FileEntry m a -> Source m FilePath
find = findFilePaths defaultFindOptions

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles'.
test :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
test matcher path = Cond.test matcher (newFileEntry path 0 defaultFindOptions)

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles', but do not follow symlinks.
ltest :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
ltest matcher path =
    Cond.test (lstat >> matcher)
        (newFileEntry path 0 defaultFindOptions { findFollowSymlinks = False })
