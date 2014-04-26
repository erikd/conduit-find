{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Data.Conduit.Find
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "basic tests" $ do
        it "file passes a test" $ do
            res <- test (regular >> not_ executable) "README.md"
            res `shouldBe` True

        it "file fails a test" $ do
            res <- test (not_ regular) "README.md"
            res `shouldBe` False

        it "file fails another test" $ do
            res <- test (regular >> executable) "README.md"
            res `shouldBe` False

        it "finds files" $ do
            xs <- runResourceT $
                find "."
                    (    ignoreVcs
                     >> pruneWhen_ (filename_ (== "dist"))
                     >> glob "*.hs"
                     >> not_ (glob "Setup*")
                     >> regular
                     >> not_ executable
                    )
                    $$ sinkList

            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist/setup-config" `elem` xs `shouldBe` False
            "./Setup.hs" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False

        it "finds files with a different ordering" $ do
            xs <- runResourceT $
                find "."
                    (    ignoreVcs
                     >> glob "*.hs"
                     >> not_ (glob "Setup*")
                     -- This pruneWhen_ only applies to .hs files now, so it won't
                     -- match anything, thus having no effect but burning CPU!
                     >> pruneWhen_ (filename_ (== "dist"))
                     >> regular
                     >> not_ executable
                    )
                    $$ sinkList

            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist/setup-config" `elem` xs `shouldBe` False
            "./Setup.hs" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False

        it "finds files using a pre-pass filter" $ do
            xs <- runResourceT $
                findRaw "." True
                    (do ignoreVcs
                        pruneWhen_ (filename_ (== "dist"))
                        glob "*.hs"
                        not_ (glob "Setup*")
                        stat
                        regular
                        not_ executable)
                    =$ mapC entryPath $$ sinkList

            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist/setup-config" `elem` xs `shouldBe` False
            "./Setup.hs" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False

        it "properly applies post-pass pruning" $ do
            xs <- runResourceT $
                findRaw "." True
                    (do ignoreVcs
                        pruneWhen_ (depth (>=1))
                        pruneWhen_ (filename_ (== "dist"))
                        glob "*.hs"
                        not_ (glob "Setup*")
                        stat
                        regular
                        not_ executable)
                    =$ mapC entryPath $$ sinkList

            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` False
            "./dist/setup-config" `elem` xs `shouldBe` False
            "./Setup.hs" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False
