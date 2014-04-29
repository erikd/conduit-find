module Main where

import Conduit
import Control.Monad
import Control.Monad.Reader.Class
import Data.Conduit.Find
import Data.List
import Filesystem.Path.CurrentOS
import System.Environment
import System.Posix.Process

main :: IO ()
main = do
    [command, dir] <- getArgs
    case command of
        "conduit" -> do
            putStrLn "Running sourceDirectoryDeep from conduit-extra"
            runResourceT $
                sourceDirectoryDeep False (decodeString dir)
                    =$ filterC ((".hs" `isSuffixOf`) . encodeString)
                    $$ mapM_C (liftIO . putStrLn . encodeString)

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
                    (decodeString dir) (return ())
                    =$ filterC ((".hs" `isSuffixOf`) . entryPath . fst)
                    $$ mapM_C (liftIO . putStrLn . entryPath . fst)

        "find" -> do
            putStrLn "Running GNU find"
            executeFile "find" True [dir, "-name", "*.hs", "-print"] Nothing
