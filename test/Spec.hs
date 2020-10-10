module Main (main) where

import Test.Hspec (hspec)

import Test.Prolens (unitSpecs)
import Test.Prolens.Inspection (inspectionSpec)


main :: IO ()
main = hspec $ do
    unitSpecs
    inspectionSpec
