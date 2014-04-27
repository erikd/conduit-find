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
      findFiles
    , findFiles'
    , lfindFiles
    , lfindFiles'
    , stat
    , lstat
    , test
    , findRaw


      -- * File path predicates
    , ignoreVcs
    , regex
    , glob
    , name_
    , filename_
    , filenameS_
    , filepath_
    , filepathS_
    , withPath

    -- * File entry predicates (uses stat information)
    , regular
    , directory
    , hasMode
    , executable
    , depth
    , lastAccessed
    , lastModified
    , withFileStatus

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
import           Data.Foldable hiding (elem)
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

type Predicate m a = CondT a m ()

data FileEntry = FileEntry
    { entryPath   :: FilePath
    , entryDepth  :: Int
    , entryStatus :: Maybe FileStatus
      -- ^ This is Nothing until we determine stat should be called.
    }

instance Show FileEntry where
    show entry = "FileEntry "
              ++ show (entryPath entry)
              ++ " " ++ show (entryDepth entry)

newFileEntry :: FilePath -> Int -> FileEntry
newFileEntry p d = FileEntry p d Nothing

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: Monad m => Predicate m FileEntry
ignoreVcs = when_ (filename_ (`elem` vcsDirs)) reject
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

regex :: Monad m => Text -> Predicate m FileEntry
regex pat = filename_ (=~ pat)

name_ :: Monad m => FilePath -> Predicate m FileEntry
name_ = filename_ . (==)

-- | This is a re-export of 'Text.Regex.Posix.=~', with the types changed for
--   ease of use with this module.  For example, you can simply say:
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

-- | Find every entry whose filename part matching the given filename globbing
--   expression.  For example: @glob "*.hs"@.
glob :: Monad m => Text -> Predicate m FileEntry
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

doStat :: MonadIO m => (String -> IO FileStatus) -> Predicate m FileEntry
doStat getstatus = do
    entry <- get
    s <- liftIO $ getstatus (encodeString (entryPath entry))
    put $ entry { entryStatus = Just s }

lstat :: MonadIO m => Predicate m FileEntry
lstat = doStat getSymbolicLinkStatus

stat :: MonadIO m => Predicate m FileEntry
stat = doStat getFileStatus

getStatus :: FileEntry -> FileStatus
getStatus e = fromMaybe
    (error $ "FileStatus has not been determined for: " ++ show (entryPath e))
    (entryStatus e)

withFileStatus :: Monad m
               => (FileStatus -> m Bool)
               -> Predicate m FileEntry
withFileStatus f = guardM_ (f . getStatus)

status :: Monad m => (FileStatus -> Bool) -> Predicate m FileEntry
status f = withFileStatus (return . f)

regular :: Monad m => Predicate m FileEntry
regular = status isRegularFile

directory :: Monad m => Predicate m FileEntry
directory = status isDirectory

hasMode :: Monad m => FileMode -> Predicate m FileEntry
hasMode m = status (\s -> fileMode s .&. m /= 0)

executable :: Monad m => Predicate m FileEntry
executable = hasMode ownerExecuteMode

withPath :: Monad m
         => (FilePath -> m Bool)
         -> Predicate m FileEntry
withPath f = guardM_ (f . entryPath)

filename_ :: Monad m => (FilePath -> Bool) -> Predicate m FileEntry
filename_ f = withPath (return . f . filename)

filenameS_ :: Monad m => (String -> Bool) -> Predicate m FileEntry
filenameS_ f = withPath (return . f . encodeString . filename)

filepath_ :: Monad m => (FilePath -> Bool) -> Predicate m FileEntry
filepath_ f = withPath (return . f)

filepathS_ :: Monad m => (String -> Bool) -> Predicate m FileEntry
filepathS_ f = withPath (return . f . encodeString)

depth :: Monad m => (Int -> Bool) -> Predicate m FileEntry
depth f = guard_ (f . entryDepth)

withStatusTime :: Monad m
               => (UTCTime -> Bool) -> (FileStatus -> POSIXTime)
               -> Predicate m FileEntry
withStatusTime f g = status (f . posixSecondsToUTCTime . g)

lastAccessed :: Monad m => (UTCTime -> Bool) -> Predicate m FileEntry
lastAccessed = flip withStatusTime accessTimeHiRes

lastModified :: Monad m => (UTCTime -> Bool) -> Predicate m FileEntry
lastModified = flip withStatusTime modificationTimeHiRes

-- | A raw find does no processing on the FileEntry, leaving it up to the user
--   to determine when and if stat should be called.  Note that unless you
--   take care to indicate when recursion should happen, an error will result
--   when the raw finder attempts to recurse on a non-directory.  The bare
--   minimum for a proper finder should look like this for non-recursion:
--
-- @
-- findRaw \<path\> $ do
--     \<apply predicates needing only pathname or depth\>
--     localM stat $ do
--         directory ||: norecurse
--         \<apply predicates needing stat info\>
-- @
--
-- To apply predicates only to a single directory, without recursing, simply
-- start (or end) the predicate with 'norecurse', and use @localM stat@ or
-- @localM lstat@ at the point where you need 'FileStatus' information.
findRaw :: (MonadIO m, MonadResource m)
        => FilePath -> Bool -> CondT FileEntry m b -> Source m (FileEntry, b)
findRaw startPath follow = go (newFileEntry startPath 0) . hoist lift
  where
    statFile = liftIO
             . (if follow
                then getFileStatus
                else getSymbolicLinkStatus)
             . encodeString

    go x pr = applyCondT x pr $ \a@(FileEntry path d mstat) mb mcond -> do
        -- If the item matched, also yield the predicate's result value.
        forM_ mb $ yield . (a,)

        -- If the conditional matched, we are requested to recurse if this
        -- is a directory
        forM_ mcond $ \cond -> do

            -- If no status has been determined, we must do so now in order
            -- to know whether to actually recurse or not.
            st <- maybe (statFile path) return mstat

            when (isDirectory st) $
                (sourceDirectory path =$) $ awaitForever $ \fp ->
                    mapInput (const ()) (const Nothing) $
                        go (newFileEntry fp (succ d)) cond

basicFind :: (MonadIO m, MonadResource m)
          => Predicate m FileEntry
          -> Bool
          -> FilePath
          -> Predicate m FileEntry
          -> Source m FileEntry
basicFind f follow path pr = findRaw path follow (f >> pr) =$= mapC fst

findFiles' :: (MonadIO m, MonadResource m)
      => FilePath -> Predicate m FileEntry
      -> Source m FileEntry
findFiles' = basicFind stat True

findFiles :: (MonadIO m, MonadResource m)
     => FilePath -> Predicate m FileEntry
     -> Source m FilePath
findFiles path pr = findFiles' path pr =$= mapC entryPath

lfindFiles' :: (MonadIO m, MonadResource m)
       => FilePath -> Predicate m FileEntry
       -> Source m FileEntry
lfindFiles' = basicFind lstat False

lfindFiles :: (MonadIO m, MonadResource m)
      => FilePath -> Predicate m FileEntry
      -> Source m FilePath
lfindFiles path pr = lfindFiles' path pr =$= mapC entryPath

-- | Test a file path using the same type of 'Predicate' that is accepted by
--   'findFiles'.
test :: MonadIO m => Predicate m FileEntry -> FilePath -> m Bool
test matcher path = Cond.test (stat >> matcher) (newFileEntry path 0)
