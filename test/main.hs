{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Control.Arrow
import Data.Conduit.Find
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "basic tests" $ do
        it "file passes a test" $ do
            res <- test (regular >>> not_ executable) "README.md"
            res `shouldBe` True

        it "file fails a test" $ do
            res <- test (not_ regular) "README.md"
            res `shouldBe` False

        it "file fails another test" $ do
            res <- test (regular >>> executable) "README.md"
            res `shouldBe` False

        it "finds files" $ do
            xs <- runResourceT $
                find "."
                    (    ignoreVcs
                     >>> prune (filename_ "dist")
                     >>> glob "*.hs"
                     >>> not_ (glob "Setup*")
                     >>> regular
                     >>> not_ executable
                    )
                    $$ sinkList

            liftIO $ putStrLn $ "foo main.hs:37.."
            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            liftIO $ putStrLn $ "foo main.hs:39.."
            "./dist/setup-config" `elem` xs `shouldBe` False
            liftIO $ putStrLn $ "foo main.hs:41.."
            "./Setup.hs" `elem` xs `shouldBe` False
            liftIO $ putStrLn $ "foo main.hs:43.."
            "./.git/config" `elem` xs `shouldBe` False

        it "finds files with a different ordering" $ do
            xs <- runResourceT $
                find "."
                    (    ignoreVcs
                     >>> glob "*.hs"
                     >>> not_ (glob "Setup*")
                     >>> prune (filename_ "dist")
                     >>> regular
                     >>> not_ executable
                    )
                    $$ sinkList

            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist/setup-config" `elem` xs `shouldBe` False
            "./Setup.hs" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False

        it "finds files using a pre-pass filter" $ do
            xs <- runResourceT $
                findWithPreFilter "." True
                    (    ignoreVcs
                     >>> glob "*.hs"
                     >>> not_ (glob "Setup*")
                     >>> prune (filename_ "dist")
                    )
                    (    regular
                     >>> not_ executable
                    )
                    =$ mapC getFilePath $$ sinkList

            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist/setup-config" `elem` xs `shouldBe` False
            "./Setup.hs" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False
