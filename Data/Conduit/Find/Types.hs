{-# LANGUAGE TypeFamilies #-}

module Data.Conduit.Find.Types where

import           Data.ByteString (ByteString)
import           Data.Text
import           Data.Text.Encoding
import           Filesystem.Path.CurrentOS
import           Foreign
import           Prelude hiding (FilePath)
import qualified System.FilePath as FP
import           System.Posix.ByteString.FilePath
import           System.PosixCompat.Files

data FindOptions = FindOptions
    { findFollowSymlinks     :: !Bool
    , findContentsFirst      :: !Bool
    , findIgnoreErrors       :: !Bool
    , findIgnoreResults      :: !Bool
    , findPreloadDirectories :: !Bool
    , findDepthFirst         :: !Bool
    }

defaultFindOptions :: FindOptions
defaultFindOptions = FindOptions
    { findFollowSymlinks     = False
    , findContentsFirst      = False
    , findIgnoreErrors       = False
    , findIgnoreResults      = False
    , findPreloadDirectories = False
    , findDepthFirst         = True
    }

data FileEntry f = FileEntry
    { entryPath        :: !RawFilePath
    , entryDepth       :: !Int
    , entryFindOptions :: !FindOptions
    , entryStatus      :: !(Maybe FileStatus)
      -- ^ This is Nothing until we determine stat should be called.
    }

newFileEntry :: RawFilePath -> Int -> FindOptions -> FileEntry f
newFileEntry fp d f = FileEntry fp d f Nothing

instance Show (FileEntry f) where
    show entry = "FileEntry "
        ++ show (entryPath entry) ++ " " ++ show (entryDepth entry)

class IsFilePath a where
    getRawFilePath :: a -> RawFilePath
    getFilePath    :: a -> FilePath
    getStrFilePath :: a -> FP.FilePath

    fromRawFilePath :: RawFilePath -> a
    fromFilePath    :: FilePath -> a
    fromStrFilePath :: FP.FilePath -> a
    fromTextPath    :: Text -> a

instance IsFilePath ByteString where
    getRawFilePath = id
    getFilePath    = fromText . decodeUtf8
    getStrFilePath = unpack . decodeUtf8

    fromRawFilePath = id
    fromFilePath    = getRawFilePath
    fromStrFilePath = getRawFilePath
    fromTextPath    = encodeUtf8

instance IsFilePath FilePath where
    getRawFilePath = encodeUtf8 . pack . encodeString
    getFilePath    = id
    getStrFilePath = encodeString

    fromRawFilePath = getFilePath
    fromFilePath    = id
    fromStrFilePath = getFilePath
    fromTextPath    = fromText

instance a ~ Char => IsFilePath [a] where
    getRawFilePath = encodeUtf8 . pack
    getFilePath    = decodeString
    getStrFilePath = id

    fromRawFilePath = getStrFilePath
    fromFilePath    = getStrFilePath
    fromStrFilePath = id
    fromTextPath    = unpack
