{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Tests.AlgoSeek
import           Test.Tasty
-- import           Test.Tasty.Hedgehog as HH
-- import           Test.Tasty.Hspec    as Hspec

main :: IO ()
main = defaultMain $ testGroup "All Tests" [Tests.AlgoSeek.tests]
