{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , find'
    , lfind
    , lfind'
    , findWithPreFilter
    , readPaths
    , stat
    , lstat
    , test

    -- * File path predicates
    , ignoreVcs
    , regex
    , glob
    , filename_
    , filenameS_
    , filepath_
    , filepathS_
    , withPath
    , entryPath

    -- * File entry predicates (uses stat information)
    , regular
    , hasMode
    , executable
    , depth
    , lastAccessed
    , lastModified
    , withStatus

    -- * Predicate combinators
    , or_
    , and_
    , not_
    , prune
    , matchAll
    , ignoreAll
    , consider
    , (=~)

    -- * Types and type classes
    , FileEntry(..)
    , Predicate
    , HasFileInfo(..)
    ) where

import Conduit
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Attoparsec.Text
import Data.Bits
import Data.Conduit.Find.Looped
import Data.Monoid
import Data.Text (Text, unpack, pack)
import Data.Time
import Data.Time.Clock.POSIX
import Filesystem.Path.CurrentOS (FilePath, encodeString, filename)
import Prelude hiding (FilePath)
import System.Posix.Files
import System.Posix.Types
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

Predicates form a Category and an Arrow, so you can use Arrow-style
composition rather than Monoids if you wish.  They also form an Applicative, a
Monad and a MonadPlus.

In the Monad, the value bound over is whatever the predicate chooses to return
(most Predicates return the same FilePath they examined, however, making the
Monad less value).  Here's an example Monad: If the find takes longer than 5
minutes, abort.  We could have used 'timeout', but this is for illustration.

@
start <- liftIO getCurrentTime
find \".\" $ do
    glob \"*.hs\"

    end <- liftIO getCurrentTime
    if diffUTCTIme end start > 300
        then ignoreAll
        else matchAll
@

The Predicate Monad is a short-circuiting monad, meaning we stop as soon as it
can be determined that the user is not interested in a given file.  To access
the current file, simply bind the result value from any Predicate.  To change
the file being matched against,for whatever reason, use 'consider'.
-}

data FileInfo = FileInfo
    { infoPath  :: FilePath
    , infoDepth :: Int
    }

instance Show FileInfo where
    show info = "FileInfo " ++ show (infoPath info)
              ++ " " ++ show (infoDepth info)

data FileEntry = FileEntry
    { entryInfo   :: FileInfo
    , entryStatus :: FileStatus
    }

instance Show FileEntry where
    show entry = "FileEntry " ++ show (entryInfo entry)

class HasFileInfo a where
    getFileInfo :: a -> FileInfo

instance HasFileInfo FileInfo where
    getFileInfo = id
    {-# INLINE getFileInfo #-}

instance HasFileInfo FileEntry where
    getFileInfo = entryInfo
    {-# INLINE getFileInfo #-}

entryPath :: HasFileInfo a => a -> FilePath
entryPath = infoPath . getFileInfo

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: (MonadIO m, HasFileInfo e) => Predicate m e
ignoreVcs = prune (filename_ (`elem` vcsDirs))
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

regex :: (Monad m, HasFileInfo e) => Text -> Predicate m e
regex pat = filename_ (=~ pat)

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
glob :: (Monad m, HasFileInfo e) => Text -> Predicate m e
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

doStat :: MonadIO m
       => (String -> IO FileStatus) -> Looped m FileInfo FileEntry
doStat getstatus = Looped $ \(FileInfo p d) -> do
    s <- liftIO $ getstatus (encodeString p)
    let entry = FileEntry (FileInfo p d) s
    return $ if isDirectory s
             then KeepAndRecurse entry (doStat getstatus)
             else Keep entry

lstat :: MonadIO m => Looped m FileInfo FileEntry
lstat = doStat getSymbolicLinkStatus

stat :: MonadIO m => Looped m FileInfo FileEntry
stat = doStat getFileStatus

withStatus :: Monad m => (FileStatus -> m Bool) -> Predicate m FileEntry
withStatus f = ifM_ (f . entryStatus)

status :: Monad m => (FileStatus -> Bool) -> Predicate m FileEntry
status f = withStatus (return . f)

regular :: Monad m => Predicate m FileEntry
regular = status isRegularFile

hasMode :: Monad m => FileMode -> Predicate m FileEntry
hasMode m = status (\s -> fileMode s .&. m /= 0)

executable :: Monad m => Predicate m FileEntry
executable = hasMode ownerExecuteMode

withPath :: HasFileInfo a => Monad m => (FilePath -> m Bool) -> Predicate m a
withPath f = ifM_ (f . entryPath)

filename_ :: (Monad m, HasFileInfo e) => (FilePath -> Bool) -> Predicate m e
filename_ f = withPath (return . f . filename)

filenameS_ :: (Monad m, HasFileInfo e) => (String -> Bool) -> Predicate m e
filenameS_ f = withPath (return . f . encodeString . filename)

filepath_ :: (Monad m, HasFileInfo e) => (FilePath -> Bool) -> Predicate m e
filepath_ f = withPath (return . f)

filepathS_ :: (Monad m, HasFileInfo e) => (String -> Bool) -> Predicate m e
filepathS_ f = withPath (return . f . encodeString)

depth :: (Monad m, HasFileInfo e) => (Int -> Bool) -> Predicate m e
depth f = if_ (f . infoDepth . getFileInfo)

withStatusTime :: Monad m
               => (UTCTime -> Bool) -> (FileStatus -> POSIXTime)
               -> Predicate m FileEntry
withStatusTime f g = status (f . posixSecondsToUTCTime . g)

lastAccessed :: Monad m => (UTCTime -> Bool) -> Predicate m FileEntry
lastAccessed = flip withStatusTime accessTimeHiRes

lastModified :: Monad m => (UTCTime -> Bool) -> Predicate m FileEntry
lastModified = flip withStatusTime modificationTimeHiRes

-- Walk through the entries of a directory tree, allowing the user to specify
-- a 'Predicate' which may decides not only which entries to yield from the
-- conduit, but also which directories to follow, and how to recurse into that
-- directory by permitting the use of a subsequent 'Predicate'.
--
-- Note that the 'followSymlinks' parameter to this function has a different
-- meaning than it does for 'sourceDirectoryDeep': if @True@, symlinks are
-- never passed to the predicate, only what they point to; if @False@,
-- symlinks are never read at all.  For 'sourceDirectoryDeep', if
-- 'followSymlinks' is @False@ it only prevents directory symlinks from being
-- read.
sourceFileEntries :: MonadResource m
                  => FileInfo
                  -> Looped m FileInfo FileEntry
                  -> Producer m FileEntry
sourceFileEntries (FileInfo p d) m = sourceDirectory p =$= awaitForever f
  where
    f fp = applyLooped m (FileInfo fp d) yield $
        sourceFileEntries (FileInfo fp (succ d))

find' :: (MonadIO m, MonadResource m)
      => FilePath -> Predicate m FileEntry -> Producer m FileEntry
find' path pr = sourceFileEntries (FileInfo path 1) (stat >>> pr)

find :: (MonadIO m, MonadResource m)
     => FilePath -> Predicate m FileEntry -> Producer m FilePath
find path pr = find' path pr =$= mapC entryPath

lfind' :: (MonadIO m, MonadResource m)
       => FilePath -> Predicate m FileEntry -> Producer m FileEntry
lfind' path pr = sourceFileEntries (FileInfo path 1) (lstat >>> pr)

lfind :: (MonadIO m, MonadResource m)
      => FilePath -> Predicate m FileEntry -> Producer m FilePath
lfind path pr = lfind' path pr =$= mapC entryPath

readPaths :: (MonadIO m, MonadResource m)
          => FilePath -> Predicate m FilePath -> Producer m FilePath
readPaths path pr = sourceDirectory path =$= awaitForever f
  where
    f fp = do
        r <- lift $ runLooped pr fp
        case r of
            Ignore -> return ()
            Keep a -> yield a
            RecurseOnly _ -> return ()
            KeepAndRecurse a _ -> yield a

data FindFilter = IgnoreFile
                | ConsiderFile
                | MaybeRecurse
    deriving (Show, Eq)

-- | Run a find, but using a pre-pass filter on the FilePaths, to eliminates
--   files from consideration early and avoid calling stat on them.
doFindPreFilter :: (MonadIO m, MonadResource m)
                => FileInfo
                -> Bool
                -> Predicate m FileInfo
                -> Predicate m FileEntry
                -> Producer m FileEntry
doFindPreFilter (FileInfo path dp) follow filt pr =
    sourceDirectory path =$= awaitForever (worker (succ dp) pr)
  where
    worker d m fp = do
        let info = FileInfo fp d
        r <- lift $ runLooped filt info
        let candidate = case r of
                Ignore -> IgnoreFile
                Keep _ -> ConsiderFile
                RecurseOnly _ -> MaybeRecurse
                KeepAndRecurse _ _ -> ConsiderFile
        unless (candidate == IgnoreFile) $ do
            st <- liftIO $
                (if follow
                 then getFileStatus
                 else getSymbolicLinkStatus) (encodeString fp)
            let next = when (isDirectory st) . doFindPreFilter info follow filt
            case candidate of
                IgnoreFile   -> return ()
                MaybeRecurse -> next pr
                ConsiderFile ->
                    applyLooped m (FileEntry (FileInfo fp d) st) yield next

findWithPreFilter :: (MonadIO m, MonadResource m)
                  => FilePath
                  -> Bool
                  -> Predicate m FileInfo
                  -> Predicate m FileEntry
                  -> Producer m FileEntry
findWithPreFilter path = doFindPreFilter (FileInfo path 1)

-- | Test a file path using the same type of 'Predicate' that is accepted by
--   'find'.
test :: MonadIO m => Predicate m FileEntry -> FilePath -> m Bool
test matcher path =
    getAny `liftM` testSingle (stat >>> matcher) (FileInfo path 0) alwaysTrue
  where
    alwaysTrue = const (return (Any True))
