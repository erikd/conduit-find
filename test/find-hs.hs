{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Control.Monad
import Control.Monad.Reader.Class
import Data.Conduit.Find
import Data.Conduit.Filesystem as CF
import Data.ByteString
import qualified Data.List as L
import Filesystem.Path.CurrentOS
import System.Environment
import System.Posix.Process
import qualified Data.Text as T
import Data.Text.Encoding
import Prelude hiding (putStrLn)
import qualified Prelude as P

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
            findFiles defaultFindOptions { findFollowSymlinks = False }
                (decodeString dir) $ do
                    path <- asks entryPath
                    guard (".hs" `isSuffixOf` path)
                    norecurse
                    liftIO $ putStrLn path

        "find-conduit2" -> do
            putStrLn "Running findFiles from find-conduit"
            runResourceT $
                sourceFindFiles defaultFindOptions { findFollowSymlinks = False }
                    (encodeUtf8 (T.pack dir)) (return ())
                    =$ filterC ((".hs" `isSuffixOf`) . entryPath . fst)
                    $$ mapM_C (liftIO . putStrLn . entryPath . fst)

        "find" -> do
            putStrLn "Running GNU find"
            executeFile "find" True [dir, "-name", "*.hs", "-print"] Nothing
