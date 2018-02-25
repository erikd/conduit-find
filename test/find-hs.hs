module Main where

import           Data.Conduit ((=$), ($$))
import           Control.Monad (guard, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class (asks)
import           Data.Conduit.Find
import qualified Data.Conduit.List as DCL
import           Data.List (isSuffixOf)
import           System.Environment (getArgs)
import           System.Posix.Process (executeFile)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit.Filesystem (sourceDirectoryDeep)


main :: IO ()
main = do
    [command, dir] <- getArgs
    case command of
        "conduit" -> do
            putStrLn "Running sourceDirectoryDeep from conduit-extra"
            runResourceT $
                sourceDirectoryDeep False dir
                    =$ DCL.filter (".hs" `isSuffixOf`)
                    =$ DCL.mapM_ (liftIO . putStrLn)
                    $$ DCL.sinkNull

        "find-conduit" -> do
            putStrLn "Running findFiles from find-conduit"
            findFiles defaultFindOptions { findFollowSymlinks = False }
                dir $ do
                    path <- asks entryPath
                    void $ guard (".hs" `isSuffixOf` path)
                    norecurse
                    liftIO $ putStrLn path

        "find-conduit2" -> do
            putStrLn "Running findFiles from find-conduit"
            runResourceT $
                sourceFindFiles defaultFindOptions { findFollowSymlinks = False }
                    dir (return ())
                    =$ DCL.filter ((".hs" `isSuffixOf`) . entryPath . fst)
                    =$ DCL.mapM_ (liftIO . putStrLn . entryPath . fst)
                    $$ DCL.sinkNull

        "find" -> do
            putStrLn "Running GNU find"
            executeFile "find" True [dir, "-name", "*.hs", "-print"] Nothing

        _ ->
            putStrLn $ "Unknown command " ++ show command
