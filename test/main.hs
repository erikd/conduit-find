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
            res <- test (regular >>> executable) "README.md"
            res `shouldBe` False

        it "finds files" $ do
            xs <- runResourceT $
                find "." (ignoreVcs >>> regular
                                    >>> not_ executable
                                    >>> glob "*.hs")
                    $$ sinkList
            "./Data/Conduit/Find.hs" `elem` xs `shouldBe` True
            "./.git/config" `elem` xs `shouldBe` False
