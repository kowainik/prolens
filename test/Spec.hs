module Main (main) where

import Test.Hspec (hspec)

import Test.Prolens (unitSpecs)
import Test.Prolens.Inspection (inspectionSpec)
import Test.Prolens.Property (lensPropertySpecs)


main :: IO ()
main = hspec $ do
    unitSpecs
    inspectionSpec
    lensPropertySpecs
