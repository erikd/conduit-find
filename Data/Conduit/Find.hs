{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Conduit.Find
    ( FileEntry(..)
    , Predicate(..)
    , sourceFileEntries
    , matchAll
    , ignoreVcs
    , regexMatcher
    , regex
    , glob
    ) where

import Conduit
import Control.Applicative
import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Foldable (for_)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text, unpack, pack)
import Filesystem.Path.CurrentOS (FilePath, encodeString, filename)
import Prelude hiding (FilePath)
import System.Posix.Files (FileStatus, getFileStatus, getSymbolicLinkStatus,
                           isDirectory)
import Text.Regex.Posix ((=~))

data FileEntry = FileEntry
    { entryPath   :: FilePath
    , entryStatus :: FileStatus
    }

newtype Predicate m =
    Predicate (FileEntry -> m (Maybe FileEntry, Maybe (Predicate m)))

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
                  => Bool -> Predicate m -> FilePath -> Producer m FileEntry
sourceFileEntries followSymlinks (Predicate matcher) dir =
    sourceDirectory dir =$= go
  where
    go = do
        mfp <- await
        for_ mfp $ \fp -> do
            stat <- liftIO $
                (if followSymlinks
                 then getFileStatus
                 else getSymbolicLinkStatus)
                (encodeString fp)
            let entry = FileEntry fp stat
            res <- lift $ matcher entry
            for_ (fst res) yield
            when (isDirectory stat) $
                for_ (snd res) $
                    flip (sourceFileEntries followSymlinks) fp
            go

-- | Return all entries.  This is the same as 'sourceDirectoryDeep', except
--   that the 'FileStatus' structure for each entry is also provided.  As a
--   result, only one stat call is ever made per entry, compared to two per
--   directory in the current version of 'sourceDirectoryDeep'.
matchAll :: Monad m => Predicate m
matchAll = Predicate $ \entry -> return (Just entry, Just matchAll)

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: MonadIO m => Predicate m
ignoreVcs = Predicate $ \entry ->
    return $ if filename (entryPath entry) `elem` vcsDirs
             then (Nothing, Nothing)
             else (Just entry, Just ignoreVcs)
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

-- | The 'regexMatcher' predicate builder matches some part of every path
--   against a given regex.  Use the simpler 'regex' if you just want to apply
--   a regex to every file name.
regexMatcher :: Monad m
             => (FilePath -> FilePath)
                -- ^ Function that specifies which part of the pathname to
                --   match against.  Use this to match against only filenames,
                --   or to relativize the path against the search root before
                --   comparing.
             -> Bool
                -- ^ If True, prune directories from the search that do not
                --   match.
             -> Text
                -- ^ The regular expression search pattern.
             -> Predicate m
regexMatcher accessor pruneNonMatching (unpack -> pat) = go
  where
    go = Predicate $ \entry ->
        return $ if encodeString (accessor (entryPath entry)) =~ pat
                 then (Just entry, Just go)
                 else (Nothing, if pruneNonMatching
                                then Nothing
                                else Just go)

-- | Find every entry whose filename part matching the given regular expression.
regex :: Monad m => Text -> Predicate m
regex = regexMatcher filename False

-- | Find every entry whose filename part matching the given filename globbing
--   expression.  For example: @glob "*.hs"@.
glob :: Monad m => Text -> Predicate m
glob g = case parseOnly globParser g of
    Left e  -> error $ "Failed to parse glob: " ++ e
    Right x -> regex ("^" <> x <> "$")
  where
    globParser :: Parser Text
    globParser = fmap mconcat $ many $
            char '*' *> return ".*"
        <|> char '?' *> return "."
        <|> (\x y z -> pack ((x:y) ++ [z]))
                <$> char '['
                -- jww (2014-04-23): This does not yet handle the pattern []],
                -- which is legal.
                <*> manyTill anyChar (try (char ']'))
                <*> char ']'
        <|> do
            x <- anyChar
            return . pack $ if x `elem` ['.', '(', ')', '^', '$']
                            then ['\\', x]
                            else [x]
