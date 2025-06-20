{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Conduit
import Data.Conduit.Find
import Data.Conduit.List qualified as DCL
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
            xs <- runConduitRes $
                find "."
                    (do ignoreVcs
                        glob "*.hs"
                        regular
                        not_ executable)
                    .| DCL.consume

            "./src/Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist-newstyle/setup-config" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False

        it "finds files with a different ordering" $ do
            xs <- runConduitRes $
                find "."
                    (do ignoreVcs
                        glob "*.hs"
                        not_ (glob "Setup*")
                        -- This applies only to .hs files now, so it won't
                        -- match anything, thus having no effect but burning
                        -- CPU!
                        when_ (name_ "dist-newstyle") prune
                        regular
                        not_ executable)
                    .| DCL.consume

            "./src/Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist-newstyle/setup-config" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False

        it "finds files using a pre-pass filter" $ do
            xs <- runConduitRes $
                find "."
                    (do ignoreVcs
                        when_ (name_ "dist-newstyle") prune
                        glob "*.hs"
                        not_ (glob "Setup*")
                        regular
                        not_ executable)
                    .| DCL.consume

            "./src/Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./dist-newstyle/setup-config" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False

        it "properly applies post-pass pruning" $ do
            xs <- runConduitRes $
                find "."
                    (do ignoreVcs
                        maxdepth_ 1
                        when_ (name_ "dist") prune
                        glob "*.hs"
                        not_ (glob "Setup*")
                        regular
                        not_ executable)
                    .| DCL.consume

            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` False
            "./dist/setup-config" `elem` xs `shouldBe` False
            "./Setup.hs" `elem` xs `shouldBe` False
            "./.git/config" `elem` xs `shouldBe` False
