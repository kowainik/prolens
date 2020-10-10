module Test.Prolens.Property
    ( lensPropertySpecs
    ) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

import Prolens
import Test.Data (genHaskeller, genName, nameL)


lensPropertySpecs :: Spec
lensPropertySpecs = describe "Lens Laws" $ do
    it "view lens (set lens value source) ≡ value" $ hedgehog $ do
        source <- forAll genHaskeller
        value <- forAll genName
        view nameL (set nameL value source) === value
    it "set lens (view lens source) source ≡ source" $ hedgehog $ do
        source <- forAll genHaskeller
        set nameL (view nameL source) source === source
    it "set lens valueNew (set lens value source) ≡ set lens valueNew source" $ hedgehog $ do
        source <- forAll genHaskeller
        value <- forAll genName
        valueNew <- forAll genName
        set nameL valueNew (set nameL value source) === set nameL valueNew source
