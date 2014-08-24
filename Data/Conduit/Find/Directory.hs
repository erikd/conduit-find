{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Find.Directory where

import           Conduit.Simple
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import qualified Data.ByteString as B
import           Data.Conduit.Find.Types
import           Data.Maybe (fromMaybe)
import           Foreign
import           Foreign.C
import           Prelude hiding (FilePath)
import           System.Posix.ByteString.FilePath
import           System.Posix.Files.ByteString

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

getDirectoryContentsAndAttrs :: RawFilePath -> IO [(RawFilePath, CUInt)]
getDirectoryContentsAndAttrs path = do
    resetErrno
    bracket
        (openDirStream path)
        closeDirStream
        (allocaBytes (fromIntegral c_sizeof_dirent) . readDir [])
  where
    readDir !acc ds direntp = do
        res <- readDirStream ds direntp
        case fst res of
            ""   -> return acc
            "."  -> readDir acc ds direntp
            ".." -> readDir acc ds direntp
            _    -> readDir (res:acc) ds direntp

sourceDirectory :: MonadBaseControl IO m
                => RawFilePath -> Source m (RawFilePath, CUInt)
sourceDirectory dir = source $ \z yield -> do
    liftBaseOp (bracket (openDirStream dir) closeDirStream) (go z yield)
  where
    go z yield ds = loop z
      where
        loop r = do
            res <- liftBase $ readDirStream ds nullPtr
            case fst res of
                ""   -> return r
                "."  -> loop r
                ".." -> loop r
                _    -> yield r res >>= loop

-- | @readDirStream dp@ calls @readdir@ to obtain the next directory entry
--   (@struct dirent@) for the open directory stream @dp@, and returns the
--   @d_name@ member of that structure.
readDirStream :: DirStream -> Ptr CDirent -> IO (RawFilePath, CUInt)
readDirStream dirp direntp = alloca loop
  where
    noresult = (B.empty, 0)

    loop ptr_dEnt = do
        r <- c_readdir_r dirp direntp ptr_dEnt
        if r == 0
            then do
                dEnt <- peek ptr_dEnt
                if dEnt == nullPtr
                    then return noresult
                    else readEntry dEnt
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
        !len   <- fromIntegral <$> d_namlen dEnt
        !entry <- d_name dEnt >>= \p -> peekFilePathLen (p, len)

        -- We can sometimes use "leaf optimization" on Linux to answer this
        -- question without performing a stat call.  This is possible because
        -- the link count of a directory is two more than the number of
        -- sub-directories it contains, so we've seen that many
        -- sub-directories, the remaining entries must be files.
        !typ <- d_type dEnt
        return (entry, typ)

statIsDirectory :: RawFilePath -> IO Bool
statIsDirectory path =
    maybe False isDirectory <$> statFilePath True True path

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
getStat :: Maybe Bool -> FileEntry f -> IO (Maybe (FileStatus, FileEntry f))
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
  c_readdir :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_readdir_r"
  c_readdir_r :: Ptr CDir -> Ptr CDirent -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_sizeof_dirent"
  c_sizeof_dirent :: CUInt

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
