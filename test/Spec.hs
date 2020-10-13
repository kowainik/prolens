module Main (main) where

import Test.Hspec (hspec)

import Test.Prolens (unitSpecs)
import Test.Prolens.Inspection (inspectionSpecs)
import Test.Prolens.Property (lensPropertySpecs, typeclassesPropertySpecs)


main :: IO ()
main = hspec $ do
    unitSpecs
    inspectionSpecs
    lensPropertySpecs
    typeclassesPropertySpecs
