{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Conduit.Find
    ( FileEntry(..)
    , Predicate
    , HasFileInfo(..)
    , consider
    , entryPath
    , matchAll
    , ignoreAll
    , ignoreVcs
    , regexMatcher
    , regex
    , glob
    , stat
    , lstat
    , regular
    , hasMode
    , executable
    , filename_
    , filenameS_
    , filepath_
    , filepathS_
    , depth
    , lastAccessed
    , lastModified
    , withPath
    , withStatus
    , prune
    , test
    , find
    , find'
    , lfind
    , lfind'
    , findWithPreFilter
    , readPaths
    , or_
    , and_
    , not_
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
import Text.Regex.Posix ((=~))

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

-- | Walk through the entries of a directory tree, allowing the user to
--   specify a 'Predicate' which may decides not only which entries to yield
--   from the conduit, but also which directories to follow, and how to
--   recurse into that directory by permitting the use of a subsequent
--   'Predicate'.
--
--   Note that the 'followSymlinks' parameter to this function has a different
--   meaning than it does for 'sourceDirectoryDeep': if @True@, symlinks are
--   never passed to the predicate, only what they point to; if @False@,
--   symlinks are never read at all.  For 'sourceDirectoryDeep', if
--   'followSymlinks' is @False@ it only prevents directory symlinks from
--   being read.
sourceFileEntries :: MonadResource m
                  => FileInfo
                  -> Looped m FileInfo FileEntry
                  -> Producer m FileEntry
sourceFileEntries (FileInfo p d) m = sourceDirectory p =$= awaitForever f
  where
    f fp = applyLooped m (FileInfo fp d) yield $
        sourceFileEntries (FileInfo fp (succ d))

entryPath :: HasFileInfo a => a -> FilePath
entryPath = infoPath . getFileInfo

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: (MonadIO m, HasFileInfo e) => Predicate m e
ignoreVcs = Looped $ \entry ->
    return $ if filename (entryPath entry) `elem` vcsDirs
             then Ignore
             else KeepAndRecurse entry ignoreVcs
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

-- | The 'regexMatcher' predicate builder matches some part of every path
--   against a given regex.  Use the simpler 'regex' if you just want to apply
--   a regex to every file name.
regexMatcher :: (Monad m, HasFileInfo e)
             => (FilePath -> FilePath)
                -- ^ Function that specifies which part of the pathname to
                --   match against.  Use this to match against only filenames,
                --   or to relativize the path against the search root before
                --   comparing.
             -> Text
                -- ^ The regular expression search pattern.
             -> Predicate m e
regexMatcher accessor (unpack -> pat) = go
  where
    pathStr = encodeString . accessor . entryPath
    go = Looped $ \entry ->
        return $ if pathStr entry =~ pat
                 then KeepAndRecurse entry go
                 else RecurseOnly go

-- | Find every entry whose filename part matching the given regular expression.
regex :: (Monad m, HasFileInfo e) => Text -> Predicate m e
regex pat = filenameS_ (=~ unpack pat)

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

test :: MonadIO m => Predicate m FileEntry -> FilePath -> m Bool
test matcher path =
    getAny `liftM` testSingle (stat >>> matcher) (FileInfo path 0) alwaysTrue
  where
    alwaysTrue = const (return (Any True))

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
