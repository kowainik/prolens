module Main (main) where

import Test.Hspec (hspec)

import Test.Prolens (unitSpecs)


main :: IO ()
main = hspec -- $ do
    unitSpecs
