module Test.Prolens
    ( unitSpecs
    ) where

import Data.Function ((&))
import Test.Hspec (Spec, describe, it, shouldBe)

import Prolens (set, view, (.~), (^.))
import Test.Data (me, nameL)


unitSpecs :: Spec
unitSpecs = describe "Prolens unit tests" $ do
    describe "getter" $ do
        it "should get name" $
            view nameL me `shouldBe` "Veronika"
        it "should get name with ^." $
            (me ^. nameL) `shouldBe` "Veronika"
    describe "setter" $ do
        it "should set name with .~" $
            view nameL (set nameL "Dmitrii" me) `shouldBe` "Dmitrii"
        it "should set name" $
            (me & nameL .~ "Dmitrii") ^. nameL `shouldBe` "Dmitrii"
