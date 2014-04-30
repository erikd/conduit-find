{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Conduit
import           Control.Monad
import           Control.Monad.Reader.Class
import           Data.ByteString hiding (putStrLn)
import           Data.ByteString.Char8 (putStrLn)
import           Data.Conduit.Filesystem as CF
import           Data.Conduit.Find
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text.Encoding
import           Filesystem.Path.CurrentOS
import qualified Prelude as P
import           Prelude hiding (putStrLn)
import           System.Environment
import           System.Posix.Process

main :: IO ()
main = do
    [command, dir] <- getArgs
    case command of
        "conduit" -> do
            putStrLn "Running sourceDirectoryDeep from conduit-extra"
            runResourceT $
                CF.sourceDirectoryDeep False dir
                    =$ filterC (".hs" `L.isSuffixOf`)
                    $$ mapM_C (liftIO . P.putStrLn)

        "find-conduit" -> do
            putStrLn "Running findFiles from find-conduit"
            findFiles
                defaultFindOptions
                    { findFollowSymlinks = False
                    }
                (decodeString dir) $ do
                    path <- asks entryPath
                    guard (".hs" `isSuffixOf` path)
                    norecurse
                    liftIO $ putStrLn path

        "find-conduit-preload" -> do
            putStrLn "Running findFiles from find-conduit"
            findFiles
                defaultFindOptions
                    { findFollowSymlinks     = False
                    , findPreloadDirectories = True
                    }
                (decodeString dir) $ do
                    path <- asks entryPath
                    guard (".hs" `isSuffixOf` path)
                    norecurse
                    liftIO $ putStrLn path

        "find-conduit-io" -> do
            putStrLn "Running ioFindFiles from find-conduit"
            ioFindFiles
                defaultFindOptions { findFollowSymlinks = False }
                (encodeUtf8 (T.pack dir)) $ do
                    path <- asks entryPath
                    guard (".hs" `isSuffixOf` path)
                    norecurse
                    liftIO $ putStrLn path

        -- "find-conduit-gather" -> do
        --     putStrLn "Running parFindFiles from find-conduit"
        --     gatherFrom 128 (\queue ->
        --             ioFindFiles
        --                 defaultFindOptions { findFollowSymlinks = False }
        --                 (encodeUtf8 (T.pack dir)) $ do
        --                     path <- asks entryPath
        --                     guard (".hs" `isSuffixOf` path)
        --                     norecurse
        --                     liftIO $ atomically $ writeTBQueue queue path)
        --         $$ mapM_C (liftIO . putStrLn)

        "find-conduit-source" -> do
            putStrLn "Running findFiles from find-conduit"
            runResourceT $
                sourceFindFiles defaultFindOptions { findFollowSymlinks = False }
                    (encodeUtf8 (T.pack dir)) (return ())
                    =$ filterC ((".hs" `isSuffixOf`) . entryPath . fst)
                    $$ mapM_C (liftIO . putStrLn . entryPath . fst)

        "find" -> do
            putStrLn "Running GNU find"
            executeFile "find" True [dir, "-name", "*.hs", "-print"] Nothing
