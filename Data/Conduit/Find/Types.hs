module Data.Conduit.Find.Types where

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
    , findLeafOptimization   :: !Bool
    , findPreloadDirectories :: !Bool
    }

defaultFindOptions :: FindOptions
defaultFindOptions = FindOptions
    { findFollowSymlinks     = True
    , findContentsFirst      = False
    , findIgnoreErrors       = False
    , findIgnoreResults      = False
    , findLeafOptimization   = True
    , findPreloadDirectories = False
    }

data FileEntry = FileEntry
    { entryPath        :: !RawFilePath
    , entryDepth       :: !Int
    , entryFindOptions :: !FindOptions
    , entryStatus      :: !(Maybe FileStatus)
      -- ^ This is Nothing until we determine stat should be called.
    }

newFileEntry :: RawFilePath -> Int -> FindOptions -> FileEntry
newFileEntry fp d f = FileEntry fp d f Nothing

instance Show FileEntry where
    show entry = "FileEntry "
        ++ show (entryPath entry) ++ " " ++ show (entryDepth entry)

class IsFilePath a where
    getRawFilePath :: a -> RawFilePath
    getFilePath    :: a -> FilePath
    getStrFilePath :: a -> FP.FilePath

instance IsFilePath RawFilePath where
    getRawFilePath = id
    getFilePath    = fromText . decodeUtf8
    getStrFilePath = unpack . decodeUtf8

instance IsFilePath FilePath where
    getRawFilePath = encodeUtf8 . pack . encodeString
    getFilePath    = id
    getStrFilePath = encodeString

instance IsFilePath FP.FilePath where
    getRawFilePath = encodeUtf8 . pack
    getFilePath    = decodeString
    getStrFilePath = id
