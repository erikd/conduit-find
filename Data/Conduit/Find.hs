{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Conduit.Find
    ( FileEntry(..)
    , Predicate
    , HasFilePath(..)
    , sourceFileEntries
    , matchAll
    , ignoreVcs
    , regexMatcher
    , regex
    , glob
    , stat
    , lstat
    , getPath
    , regular
    , executable
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
import Data.Foldable (for_)
import Data.Monoid
import Data.Text (Text, unpack, pack)
import Filesystem.Path.CurrentOS (FilePath, encodeString, filename)
import Prelude hiding (FilePath)
import System.Posix.Files
import Text.Regex.Posix ((=~))
import Debug.Trace

data FileEntry = FileEntry
    { entryPath   :: FilePath
    , entryStatus :: FileStatus
    }

instance Show FileEntry where
    show entry = "FileEntry " ++ show (entryPath entry)

class HasFilePath a where
    getFilePath :: a -> FilePath

instance HasFilePath FilePath where
    getFilePath = id

instance HasFilePath FileEntry where
    getFilePath = entryPath

type Predicate m a = Looped m a a

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
                  => Looped m FilePath FileEntry
                  -> FilePath
                  -> Producer m FileEntry
sourceFileEntries matcher dir = sourceDirectory (trace ("dir: " ++ show (dir)) $ dir) =$= go matcher
  where
    go m = do
        mfp <- await
        for_ mfp $ \fp -> do
            applyPredicate m (trace ("fp: " ++ show (fp)) $ fp) yield (flip sourceFileEntries fp)
            go m

-- | Return all entries.  This is the same as 'sourceDirectoryDeep', except
--   that the 'FileStatus' structure for each entry is also provided.  As a
--   result, only one stat call is ever made per entry, compared to two per
--   directory in the current version of 'sourceDirectoryDeep'.
matchAll :: Monad m => Predicate m a
matchAll = Looped $ \entry -> return $ KeepAndRecurse entry matchAll

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: (MonadIO m, HasFilePath e) => Predicate m e
ignoreVcs = Looped $ \entry ->
    return $ if filename (getFilePath entry) `elem` vcsDirs
             then Ignore
             else KeepAndRecurse entry ignoreVcs
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

-- | The 'regexMatcher' predicate builder matches some part of every path
--   against a given regex.  Use the simpler 'regex' if you just want to apply
--   a regex to every file name.
regexMatcher :: (Monad m, HasFilePath e)
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
    go = Looped $ \entry ->
        return $ if encodeString (accessor (getFilePath entry)) =~ pat
                 then trace ("match yes") $ KeepAndRecurse entry go
                 else trace ("match no: " ++ pat) $ Recurse entry go

-- | Find every entry whose filename part matching the given regular expression.
regex :: (Monad m, HasFilePath e) => Text -> Predicate m e
regex = regexMatcher filename

-- | Find every entry whose filename part matching the given filename globbing
--   expression.  For example: @glob "*.hs"@.
glob :: (Monad m, HasFilePath e) => Text -> Predicate m e
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
       => (String -> IO FileStatus) -> Looped m FilePath FileEntry
doStat getstatus = Looped $ \path -> do
    liftIO $ putStrLn $ "We are calling stat on " ++ encodeString path
    s <- liftIO $ getstatus (encodeString path)
    let entry = FileEntry path s
    return $ if isDirectory s
             then KeepAndRecurse entry (doStat getstatus)
             else Keep entry

lstat :: MonadIO m => Looped m FilePath FileEntry
lstat = doStat getSymbolicLinkStatus

stat :: MonadIO m => Looped m FilePath FileEntry
stat = doStat getFileStatus

getPath :: MonadIO m => Looped m FileEntry FilePath
getPath = liftLooped (return . entryPath)

status :: Monad m => (FileStatus -> Bool) -> Predicate m FileEntry
status f = if_ (f . entryStatus)

regular :: Monad m => Predicate m FileEntry
regular = status isRegularFile

executable :: Monad m => Predicate m FileEntry
executable = status (\s -> fileMode s .&. ownerExecuteMode /= 0)

prune :: (Monad m, HasFilePath e) => FilePath -> Predicate m e
prune path = Looped $ \entry ->
    return $ if getFilePath entry == path
             then trace ("prune ignore") $  Ignore
             else trace ("prune keep") $  KeepAndRecurse entry (prune path)

test :: MonadIO m => Predicate m FileEntry -> FilePath -> m Bool
test matcher path =
    getAny `liftM` testSingle (stat >>> matcher) path alwaysTrue
  where
    alwaysTrue = const (return (Any True))

find :: (MonadIO m, MonadResource m)
     => FilePath -> Predicate m FileEntry -> Producer m FilePath
find path pr = sourceFileEntries (stat >>> pr) path =$= mapC entryPath

findWithPreFilter :: (MonadIO m, MonadResource m)
                  => FilePath
                  -> Bool
                  -> Predicate m FilePath
                  -> Predicate m FileEntry
                  -> Producer m FileEntry
findWithPreFilter path followSymlinks filt pr =
    sourceFileEntries (filt >>> (if followSymlinks
                                 then stat
                                 else lstat)
                            >>> pr) path

find' :: (MonadIO m, MonadResource m)
      => FilePath -> Predicate m FileEntry -> Producer m FileEntry
find' path pr = sourceFileEntries (stat >>> pr) path

lfind :: (MonadIO m, MonadResource m)
      => FilePath -> Predicate m FileEntry -> Producer m FilePath
lfind path pr = sourceFileEntries (lstat >>> pr) path =$= mapC entryPath

lfind' :: (MonadIO m, MonadResource m)
       => FilePath -> Predicate m FileEntry -> Producer m FileEntry
lfind' path pr = sourceFileEntries (lstat >>> pr) path

readPaths :: (MonadIO m, MonadResource m)
          => FilePath -> Predicate m FilePath -> Producer m FilePath
readPaths path pr = sourceDirectory path =$= do
    mfp <- await
    for_ mfp $ \fp -> do
        r <- lift $ runLooped pr fp
        case r of
            Ignore -> return ()
            Keep a -> yield a
            Recurse _ _ -> return ()
            KeepAndRecurse a _ -> yield a
