module Data.Conduit.Find.Types where

import Foreign
import Prelude hiding (FilePath)
import System.Posix.ByteString.FilePath
import System.PosixCompat.Files

data FindOptions = FindOptions
    { findFollowSymlinks   :: !Bool
    , findContentsFirst    :: !Bool
    , findIgnoreErrors     :: !Bool
    , findIgnoreResults    :: !Bool
    , findLeafOptimization :: !Bool
    }

defaultFindOptions :: FindOptions
defaultFindOptions = FindOptions
    { findFollowSymlinks   = True
    , findContentsFirst    = False
    , findIgnoreErrors     = False
    , findIgnoreResults    = False
    , findLeafOptimization = True
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
