{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
      sourceFindFiles
    -- , find
    , findFiles
    -- , findFilePaths
    , FindOptions(..)
    , defaultFindOptions
    , test
    , ltest
    , stat
    , lstat
    , hasStatus

      -- * File path predicates
    , glob
    , regex
    , ignoreVcs

      -- * GNU find compatibility predicates
    , depth_
    , follow_
    , noleaf_
    , prune_
    , maxdepth_
    , mindepth_
    , ignoreErrors_
    , noIgnoreErrors_
    , amin_
    , atime_
    , anewer_
    , empty_
    , executable_
    , gid_
    , name_
    , getDepth
    , filename_
    , pathname_
    , getFilePath

    -- * File entry predicates (uses stat information)
    , regular
    , directory
    , hasMode
    , executable
    , lastAccessed_
    , lastModified_

    -- * Predicate combinators
    , module Cond
    , (=~)

    -- * Types and type classes
    , FileEntry(..)
    ) where

import           Conduit
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.State.Class
import           Data.Attoparsec.Text as A
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Unsafe
import           Data.Char (ord)
import qualified Data.Cond as Cond
import           Data.Cond hiding (test)
import qualified Data.Conduit.Filesystem as CF
#if LEAFOPT
import           Data.IORef
#endif
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Monoid
import qualified Data.Streaming.Filesystem as F
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Clock.POSIX
import           Foreign
import           Foreign.C
import           Filesystem.Path.CurrentOS (FilePath, toText,
                                            encodeString, decodeString)
import           Prelude hiding (FilePath)
import qualified System.FilePath as FP
import           System.Posix.ByteString.FilePath
-- import           System.Posix.Directory.ByteString hiding (readDirStream)
import           System.Posix.FilePath
import           System.PosixCompat.Files
import           System.PosixCompat.Types
import           Text.Regex.Posix ((=~))

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
              ++ show (entryPath entry)
              ++ " " ++ show (entryDepth entry)

getFilePath :: Monad m => CondT FileEntry m RawFilePath
getFilePath = gets entryPath

pathname_ :: Monad m => (RawFilePath -> Bool) -> CondT FileEntry m ()
pathname_ f = guard . f =<< getFilePath

filename_ :: Monad m => (RawFilePath -> Bool) -> CondT FileEntry m ()
filename_ f = pathname_ (f . takeFileName)

getDepth :: Monad m => CondT FileEntry m Int
getDepth = gets entryDepth

modifyFindOptions :: Monad m
                  => (FindOptions -> FindOptions)
                  -> CondT FileEntry m ()
modifyFindOptions f =
    modify $ \e -> e { entryFindOptions = f (entryFindOptions e) }

------------------------------------------------------------------------
-- Workalike options for emulating GNU find.
------------------------------------------------------------------------

depth_ :: Monad m => CondT FileEntry m ()
depth_ = modifyFindOptions $ \opts -> opts { findContentsFirst = True }

follow_ :: Monad m => CondT FileEntry m ()
follow_ = modifyFindOptions $ \opts -> opts { findFollowSymlinks = True }

noleaf_ :: Monad m => CondT FileEntry m ()
noleaf_ = modifyFindOptions $ \opts -> opts { findLeafOptimization = False }

prune_ :: Monad m => CondT a m ()
prune_ = prune

ignoreErrors_ :: Monad m => CondT FileEntry m ()
ignoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = True }

noIgnoreErrors_ :: Monad m => CondT FileEntry m ()
noIgnoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = False }

maxdepth_ :: Monad m => Int -> CondT FileEntry m ()
maxdepth_ l = getDepth >>= guard . (<= l)

mindepth_ :: Monad m => Int -> CondT FileEntry m ()
mindepth_ l = getDepth >>= guard . (>= l)

-- xdev_ = error "NYI"

timeComp :: MonadIO m
         => ((UTCTime -> Bool) -> CondT FileEntry m ()) -> Int
         -> CondT FileEntry m ()
timeComp f n = do
    now <- liftIO getCurrentTime
    f (\t -> diffUTCTime now t > fromIntegral n)

amin_ :: MonadIO m => Int -> CondT FileEntry m ()
amin_ n = timeComp lastAccessed_ (n * 60)

atime_ :: MonadIO m => Int -> CondT FileEntry m ()
atime_ n = timeComp lastAccessed_ (n * 24 * 3600)

anewer_ :: MonadIO m => RawFilePath -> CondT FileEntry m ()
anewer_ path = do
    e  <- get
    es <- applyStat Nothing
    ms <- getStat Nothing e { entryPath   = path
                            , entryStatus = Nothing
                            }
    case ms of
        Nothing     -> prune >> error "This is never reached"
        Just (s, _) -> guard $ diffUTCTime (f s) (f es) > 0
  where
    f = posixSecondsToUTCTime . realToFrac . accessTime

-- cmin_ = error "NYI"
-- cnewer_ = error "NYI"
-- ctime_ = error "NYI"

empty_ :: MonadIO m => CondT FileEntry m ()
empty_ = (regular   >> hasStatus ((== 0) . fileSize))
     <|> (directory >> hasStatus ((== 2) . linkCount))

executable_ :: MonadIO m => CondT FileEntry m ()
executable_ = executable

gid_ :: MonadIO m => Int -> CondT FileEntry m ()
gid_ n = hasStatus ((== n) . fromIntegral . fileGroup)

{-
group_ name
ilname_ pat
iname_ pat
inum_ n
ipath_ pat
iregex_ pat
iwholename_ pat
links_ n
lname_ pat
mmin_
mtime_
-}

name_ :: Monad m => RawFilePath -> CondT FileEntry m ()
name_ = filename_ . (==)

{-
newer_ path
newerXY_ ref
nogroup_
nouser_
path_ pat
perm_ mode :: Perm
readable_
regex_ pat
samefile_ path
size_ n :: Size
type_ c
uid_ n
used_ n
user_ name
wholename_ pat
writable_
xtype_ c
-}

------------------------------------------------------------------------

statFilePath :: Bool -> Bool -> RawFilePath -> IO (Maybe FileStatus)
statFilePath follow ignoreErrors path = do
    let doStat = (if follow
                  then getFileStatus
                  else getSymbolicLinkStatus) (unpack (decodeUtf8 path))
    catch (Just <$> doStat) $ \e ->
        if ignoreErrors
        then return Nothing
        else throwIO (e :: IOException)

-- | Get the current status for the file.  If the status being requested is
--   already cached in the entry information, simply return it from there.
getStat :: MonadIO m
        => Maybe Bool
        -> FileEntry
        -> m (Maybe (FileStatus, FileEntry))
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
    follow   = findFollowSymlinks . entryFindOptions
    wrapStat = liftIO $ statFilePath
        (fromMaybe (findFollowSymlinks opts) mfollow)
        (findIgnoreErrors opts)
        (entryPath entry)
      where
        opts = entryFindOptions entry

applyStat :: MonadIO m => Maybe Bool -> CondT FileEntry m FileStatus
applyStat mfollow = do
    ms <- lift . getStat mfollow =<< get
    case ms of
        Nothing      -> prune >> error "This is never reached"
        Just (s, e') -> s <$ put e'

lstat :: MonadIO m => CondT FileEntry m FileStatus
lstat = applyStat (Just False)

stat :: MonadIO m => CondT FileEntry m FileStatus
stat = applyStat (Just True)

hasStatus :: MonadIO m => (FileStatus -> Bool) -> CondT FileEntry m ()
hasStatus f = guard . f =<< applyStat Nothing

regular :: MonadIO m => CondT FileEntry m ()
regular = hasStatus isRegularFile

executable :: MonadIO m => CondT FileEntry m ()
executable = hasMode ownerExecuteMode

directory :: MonadIO m => CondT FileEntry m ()
directory = hasStatus isDirectory

hasMode :: MonadIO m => FileMode -> CondT FileEntry m ()
hasMode m = hasStatus (\s -> fileMode s .&. m /= 0)

withStatusTime :: MonadIO m
               => (FileStatus -> EpochTime) -> (UTCTime -> Bool)
               -> CondT FileEntry m ()
withStatusTime g f = hasStatus (f . posixSecondsToUTCTime . realToFrac . g)

lastAccessed_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastAccessed_ = withStatusTime accessTime

lastModified_ :: MonadIO m => (UTCTime -> Bool) -> CondT FileEntry m ()
lastModified_ = withStatusTime modificationTime

regex :: Monad m => String -> CondT FileEntry m ()
regex pat = filename_ (=~ pat)

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: Monad m => CondT FileEntry m ()
ignoreVcs = when_ (filename_ (`elem` vcsDirs)) prune
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

-- | Find every entry whose filename part matching the given filename globbing
--   expression.  For example: @glob "*.hs"@.
glob :: Monad m => String -> CondT FileEntry m ()
glob g = case parseOnly globParser (pack g) of
    Left e  -> error $ "Failed to parse glob: " ++ e
    Right x -> regex ("^" <> unpack x <> "$")
  where
    globParser :: Parser Text
    globParser = fmap mconcat $ many $
            char '*' *> return ".*"
        <|> char '?' *> return "."
        <|> string "[]]" *> return "[]]"
        <|> (\x y z -> pack ((x:y) ++ [z]))
                <$> char '['
                <*> manyTill anyChar (A.try (char ']'))
                <*> char ']'
        <|> do
            x <- anyChar
            return . pack $ if x `elem` ".()^$"
                            then ['\\', x]
                            else [x]

#if LEAFOPT
type DirCounter = IORef Int

newDirCounter :: MonadIO m => m DirCounter
newDirCounter = liftIO $ newIORef 1
#else
type DirCounter = ()

newDirCounter :: MonadIO m => m DirCounter
newDirCounter = return ()
#endif

-- | Find file entries in a directory tree, recursively, applying the given
--   recursion predicate to the search.  This conduit yields pairs of type
--   @(FileEntry, a)@, where is the return value from the predicate at each
--   step.
sourceFindFiles :: (MonadIO m, MonadResource m)
                => FindOptions
                -> RawFilePath
                -> CondT FileEntry m a
                -> Producer m (FileEntry, a)
sourceFindFiles findOptions startPath predicate = do
    startDc <- newDirCounter
    walkChildren startDc
        (newFileEntry startPath 0 findOptions)
        startPath
        (Just predicate)
  where
    sep = fromIntegral (ord '/')

    walkChildren :: MonadResource m
                 => DirCounter
                 -> FileEntry
                 -> RawFilePath
                 -> Maybe (CondT FileEntry m a)
                 -> Producer m (FileEntry, a)
    walkChildren _ _ _ Nothing = return ()
    -- If the conditional matched, we are requested to recurse if this is a
    -- directory
    walkChildren !dc !entry !path (Just !cond) = do
            let path' = if B.last path == sep
                        then path
                        else B.snoc path sep
        -- st <- lift $ checkIfDirectory dc entry path
        -- when (fmap isDirectory st == Just True) $ do
#if LEAFOPT
            -- Update directory count for the parent directory.
            liftIO $ modifyIORef dc pred
            -- Track the directory count for this child path.
            let lc = linkCount st - 2
                opts' = (entryFindOptions entry)
                    { findLeafOptimization = leafOpt && lc >= 0
                    }
            dc' <- liftIO $ newIORef lc
#else
            let dc'   = dc
                opts' = entryFindOptions entry
#endif
#if SIZEOPT
            let lc = linkCount (fromJust st) - 2
            v <- liftIO $ VM.new (fromIntegral lc)
            liftIO $ bracket
                (Dir.openDirStream path)
                Dir.closeDirStream
                (readDir v 0)
            fps <- liftIO $ V.unsafeFreeze v
            V.forM_ fps $ \fp -> do
#else
            fps <- liftIO $ bracket
                (openDirStream path)
                closeDirStream
                (readDir [])

            forM_ fps $ \fp -> do
#endif
                let fp' = B.append path' (fst fp)
                    child = newFileEntry fp'
                        (succ (entryDepth entry)) opts'

                ((!mres, !mcond), !child') <- lift $ applyCondT child cond

                let copts' = entryFindOptions child'
                    this  = unless (findIgnoreResults copts') $
                                case mres of
                                    Nothing  -> return ()
                                    Just res -> yield (child', res)
                    next  = when (snd fp) $ walkChildren dc child' fp' mcond

                if findContentsFirst copts'
                    then next >> this
                    else this >> next
      where
#if SIZEOPT
        readDir v i ds = do
            p <- readDirStream ds
            case fst p of
                ""   -> return ()
                "."  -> readDir v i ds
                ".." -> readDir v i ds
                _    -> do
                    VM.write v i p
                    readDir v (succ i) ds
#else
        readDir acc ds = do
            p <- readDirStream ds
            case fst p of
                ""   -> return acc
                "."  -> readDir acc ds
                ".." -> readDir acc ds
                _    -> readDir (p:acc) ds
#endif

    -- Return True if the given entry is a directory.  We can sometimes use
    -- "leaf optimization" on Linux to answer this question without performing
    -- a stat call.  This is possible because the link count of a directory is
    -- two more than the number of sub-directories it contains, so we've seen
    -- that many sub-directories, the remaining entries must be files.
    checkIfDirectory :: MonadResource m
                     => DirCounter
                     -> FileEntry
                     -> RawFilePath
                     -> m (Maybe FileStatus)
    checkIfDirectory !dc !entry !path = do
#if LEAFOPT
        let leafOpt = findLeafOptimization (entryFindOptions entry)
        doStat <- if leafOpt
                  then (> 0) <$> liftIO (readIORef dc)
                  else return True
#else
        let doStat = dc == () -- to quiet hlint warnings
#endif
        let opts = entryFindOptions entry
        if doStat
            then liftIO $ statFilePath
                (findFollowSymlinks opts)
                (findIgnoreErrors opts)
                path
            else return Nothing

type CDir = ()
type CDirent = ()
type DirStream = Ptr CDir

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: RawFilePath -> IO DirStream
openDirStream name =
  withFilePath name $ \s ->
    throwErrnoPathIfNullRetry "openDirStream" name $ c_opendir s

foreign import ccall unsafe "__hsunix_opendir"
   c_opendir :: CString  -> IO (Ptr CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--  structure.
readDirStream :: DirStream -> IO (RawFilePath, Bool)
readDirStream dirp =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
	 then do dEnt <- peek ptr_dEnt
		 if (dEnt == nullPtr)
                    then return (B.empty, False)
		    else do
#if 0
                     cstr <- d_name dEnt
                     len <- d_namlen dEnt
                     typ <- d_type dEnt

                     -- -- This code is slower than just peeking and copying
                     -- entry <- unsafePackCStringFinalizer
                     --     (castPtr cstr) (fromIntegral len) (c_freeDirEnt dEnt)
#else
                     len <- fromIntegral <$> d_namlen dEnt
                     entry <- (d_name dEnt >>= \p -> peekFilePathLen (p, len))
                     typ <- d_type dEnt
		     c_freeDirEnt dEnt
#endif
		     return (entry, typ .&. 4 /= 0)
	 else do errno <- getErrno
		 if (errno == eINTR) then loop ptr_dEnt else do
		 let (Errno eo) = errno
		 if (eo == 0)
                    then return (B.empty, False)
		    else throwErrno "readDirStream"

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

-- | @closeDirStream dp@ calls @closedir@ to close
--   the directory stream @dp@.
closeDirStream :: DirStream -> IO ()
closeDirStream dirp =
  throwErrnoIfMinus1Retry_ "closeDirStream" (c_closedir dirp)

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

convertPath :: FilePath -> RawFilePath
convertPath fp = either encodeUtf8 encodeUtf8 (toText fp)

findFiles :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
          => FindOptions
          -> FilePath
          -> CondT FileEntry m a
          -> m ()
findFiles opts path predicate =
    runResourceT $
        sourceFindFiles opts { findIgnoreResults = True } (convertPath path)
            (hoist lift predicate) $$ sinkNull

-- -- | A simpler version of 'findFiles', which yields only 'FilePath' values,
-- --   and ignores any values returned by the predicate action.
-- findFilePaths :: (MonadIO m, MonadResource m)
--               => FindOptions
--               -> FilePath
--               -> CondT FileEntry m a
--               -> Producer m FilePath
-- findFilePaths opts path predicate =
--     mapOutput decodeString $
--         sourceFindFiles opts path predicate =$= mapC (entryPath . fst)

-- -- | Calls 'findFilePaths' with the default set of finding options.
-- --   Equivalent to @findFilePaths defaultFindOptions@.
-- find :: (MonadIO m, MonadResource m)
--      => FilePath -> CondT FileEntry m a -> Producer m FilePath
-- find = findFilePaths defaultFindOptions

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles'.
test :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
test matcher path =
    Cond.test (newFileEntry (convertPath path) 0 defaultFindOptions) matcher

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles', but do not follow symlinks.
ltest :: MonadIO m => CondT FileEntry m () -> FilePath -> m Bool
ltest matcher path =
    Cond.test
        (newFileEntry (convertPath path) 0 defaultFindOptions
            { findFollowSymlinks = False })
        matcher
