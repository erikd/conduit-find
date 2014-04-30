{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Conduit.Find.Directory where

import           Conduit
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Conduit.Find.Types
import           Data.Maybe (fromMaybe)
import           Foreign
import           Foreign.C
import           Prelude hiding (FilePath)
import           System.Posix.ByteString.FilePath
import           System.Posix.Files.ByteString
import           System.Posix.FilePath

getDirectoryContentsAndAttrs :: RawFilePath -> Int -> Bool
                             -> IO [(RawFilePath, Maybe Bool)]
getDirectoryContentsAndAttrs path links hasDtType =
    bracket
        (openDirStream path)
        closeDirStream
        (readDir [] links)
  where
    readDir acc lc ds = do
        (p, isDir) <- readDirStream ds (lc == 0) hasDtType
        case p of
            ""   -> return acc
            "."  -> readDir acc (lc - 1) ds
            ".." -> readDir acc (lc - 1) ds
            _    -> readDir
                ((p, isDir):acc)
                (if isDir == Just True && lc >= 0
                 then lc - 1
                 else lc)
                ds

type CDir = ()
type CDirent = ()
type DirStream = Ptr CDir

-- | @openDirStream dir@ calls @opendir@ to obtain a directory stream for
--   @dir@.
openDirStream :: RawFilePath -> IO DirStream
openDirStream name = withFilePath name $ \s ->
    throwErrnoPathIfNullRetry "openDirStream" name $ c_opendir s

foreign import ccall unsafe "__hsunix_opendir"
   c_opendir :: CString  -> IO (Ptr CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the next directory entry
--   (@struct dirent@) for the open directory stream @dp@, and returns the
--   @d_name@ member of that structure.
--
-- jww (2014-04-30): Don't return Maybe Int for a directory's link count, but
-- the link count and the file type, since we can know the file type
-- statically through dt_type, and then tests like "regular" or "directory"
-- are always free.
readDirStream :: DirStream -> Bool -> Bool -> IO (RawFilePath, Maybe Bool)
readDirStream dirp noMoreDirs hasDtType = alloca loop
  where
    noresult = (B.empty, Nothing)

    loop ptr_dEnt = do
        resetErrno
        r <- c_readdir dirp ptr_dEnt
        if r == 0
            then do
                dEnt <- peek ptr_dEnt
                if dEnt == nullPtr
                    then return noresult
                    else readEntry dEnt `finally` c_freeDirEnt dEnt
            else do
                errno <- getErrno
		if errno == eINTR
                    then loop ptr_dEnt
                    else do
                        let Errno eo = errno
                        if eo == 0
                            then return noresult
                            else throwErrno "readDirStream"

    readEntry dEnt = do
        len   <- fromIntegral <$> d_namlen dEnt
        entry <- d_name dEnt >>= \p -> peekFilePathLen (p, len)

        -- We can sometimes use "leaf optimization" on Linux to answer this
        -- question without performing a stat call.  This is possible because
        -- the link count of a directory is two more than the number of
        -- sub-directories it contains, so we've seen that many
        -- sub-directories, the remaining entries must be files.
        if noMoreDirs
            then return (entry, Just False)
            else
                if hasDtType
                then do
                    typ <- d_type dEnt
                    let isDir = typ .&. 4 /= 0
                    return (entry, Just isDir)
                else
                    return (entry, Nothing)

statFilePath :: Bool -> Bool -> RawFilePath -> IO (Maybe FileStatus)
statFilePath follow ignoreErrors path = do
    let doStat = (if follow
                  then getFileStatus
                  else getSymbolicLinkStatus) path
    catch (Just <$> doStat) $ \e ->
        if ignoreErrors
        then return Nothing
        else throwIO (e :: IOException)

-- | Get the current status for the file.  If the status being requested is
--   already cached in the entry information, simply return it from there.
getStat :: Maybe Bool -> FileEntry -> IO (Maybe (FileStatus, FileEntry))
getStat mfollow entry = case entryStatus entry of
    Just s
        | maybe True (== follow entry) mfollow ->
            return $ Just (s, entry)
        | otherwise -> fmap (, entry) `liftM` wrapStat
    Nothing -> do
        ms <- wrapStat
        return $ case ms of
            Just s  -> Just (s, entry { entryStatus = Just s })
            Nothing -> Nothing
  where
    follow = findFollowSymlinks . entryFindOptions

    wrapStat = statFilePath
        (fromMaybe (findFollowSymlinks opts) mfollow)
        (findIgnoreErrors opts)
        (entryPath entry)
      where
        opts = entryFindOptions entry

-- traversing directories
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString

foreign import ccall unsafe "__hscore_d_namlen"
  d_namlen :: Ptr CDirent -> IO CUInt

foreign import ccall unsafe "__hscore_d_type"
  d_type :: Ptr CDirent -> IO CUInt

-- | @closeDirStream dp@ calls @closedir@ to close the directory stream @dp@.
closeDirStream :: DirStream -> IO ()
closeDirStream dirp =
  throwErrnoIfMinus1Retry_ "closeDirStream" (c_closedir dirp)

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

sourceDirectory :: MonadResource m
                => RawFilePath -> Int -> Bool
                -> Producer m (RawFilePath, Maybe Bool)
sourceDirectory dir links hasDtType =
    bracketP (openDirStream dir) closeDirStream go
  where
    go ds = loop links
      where
        loop lc = do
            (fp, isDir) <- liftIO $ readDirStream ds (lc == 0) hasDtType
            case fp of
                "" -> return ()
                _ -> do
                    yield (dir </> fp, isDir)
                    loop $ if isDir == Just True && lc >= 0
                           then lc - 1
                           else lc
