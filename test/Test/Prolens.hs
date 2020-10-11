module Test.Prolens
    ( unitSpecs
    ) where

import Data.Function ((&))
import Test.Hspec (Spec, describe, it, shouldBe)

import Prolens (preview, set, view, (.~), (^.))
import Test.Data (Grade (..), gradeMark, gradeOther, me, nameL, _Mark)


unitSpecs :: Spec
unitSpecs = describe "Prolens unit tests" $ do
    lensSpecs
    prismSpecs


lensSpecs :: Spec
lensSpecs = describe "Lenses" $ do
    describe "getter" $ do
        it "should get name" $
            view nameL me `shouldBe` "Veronika"
        it "should get name with ^." $
            (me ^. nameL) `shouldBe` "Veronika"
    describe "setter" $ do
        it "should set name" $
            view nameL (set nameL "Dmitrii" me) `shouldBe` "Dmitrii"
        it "should set name with .~" $
            (me & nameL .~ "Dmitrii") ^. nameL `shouldBe` "Dmitrii"


prismSpecs :: Spec
prismSpecs = describe "Prisms" $ do
    describe "preview" $ do
        it "should get mark" $
            preview _Mark gradeMark `shouldBe` Just 5
        it "should not get mark" $
            preview _Mark gradeOther `shouldBe` Nothing
    describe "set" $ do
        it "should get mark" $
            set _Mark 4 gradeMark `shouldBe` Mark 4
        it "should not get mark" $
            set _Mark 4 gradeOther `shouldBe` gradeOther
