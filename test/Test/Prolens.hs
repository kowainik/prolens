module Test.Prolens
    ( unitSpecs
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Prolens (set, view)
import Test.Data (me, nameL)


unitSpecs :: Spec
unitSpecs = describe "Prolens unit tests" $ do
    describe "getter" $
        it "should get name" $ view nameL me `shouldBe` "Veronika"
    describe "setter" $
        it "should get name" $ view nameL (set nameL "Dmitrii" me) `shouldBe` "Dmitrii"
