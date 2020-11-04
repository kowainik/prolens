{-# LANGUAGE TemplateHaskell #-}

{- | Performance tests for @prolens@. Uses the @inspection-testing@
library to make sure that lenses are as efficient as manual record
getters and update syntax.
-}

module Test.Prolens.Inspection
    ( inspectionSpecs
    ) where

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.Inspection (Result (..), hasNoTypeClasses, inspectTest, (===))

import Prolens (preview, set, view)
import Test.Data (Grade (..), Haskeller (..), Knowledge (..), _Mark, knowledgeL, nameL, syntaxL)


setNameViaLens :: Haskeller -> Haskeller
setNameViaLens = set nameL "Dmitrii"

setNameManually :: Haskeller -> Haskeller
setNameManually h = h { haskellerName = "Dmitrii" }

getNameViaLens :: Haskeller -> String
getNameViaLens = view nameL

getNameManually :: Haskeller -> String
getNameManually (Haskeller name _ _) = name

setSyntaxViaLens :: Haskeller -> Haskeller
setSyntaxViaLens = set (knowledgeL . syntaxL) True

setSyntaxManually :: Haskeller -> Haskeller
setSyntaxManually h = h { haskellerKnowledge = (haskellerKnowledge h) { kSyntax = True } }

getSyntaxViaLens :: Haskeller -> Bool
getSyntaxViaLens = view (knowledgeL . syntaxL)

getSyntaxManually :: Haskeller -> Bool
getSyntaxManually (Haskeller _ _ (Knowledge syntax _ _ _ _)) = syntax

inspectionSpecs :: Spec
inspectionSpecs = describe "Performance Inspection Testing" $ do
    lensSpecs
    prismSpecs

lensSpecs :: Spec
lensSpecs = describe "Lens" $ do
    describe "set" $ do
        it "setting single via lens is efficient as manual record update" $
            $(inspectTest $ 'setNameViaLens === 'setNameManually) `shouldSatisfy` isSuccess
        it "setting single via lens doesn't have intermediate typeclasses" $
            $(inspectTest $ hasNoTypeClasses 'setNameViaLens) `shouldSatisfy` isSuccess
        it "setting composition via lens is efficient as manual record update" $
            $(inspectTest $ 'setSyntaxViaLens === 'setSyntaxManually) `shouldSatisfy` isSuccess
        it "setting composition via lens doesn't have intermediate typeclasses" $
            $(inspectTest $ hasNoTypeClasses 'setSyntaxViaLens) `shouldSatisfy` isSuccess
    describe "view" $ do
        it "getting single via lens is efficient as plain record function" $
            $(inspectTest $ 'getNameViaLens === 'getNameManually) `shouldSatisfy` isSuccess
        it "getting single via lens doesn't have intermediate typeclasses" $
            $(inspectTest $ hasNoTypeClasses 'getNameViaLens) `shouldSatisfy` isSuccess
        it "getting composition via lens is efficient as plain record function" $
            $(inspectTest $ 'getSyntaxViaLens === 'getSyntaxManually) `shouldSatisfy` isSuccess
        it "getting composition via lens doesn't have intermediate typeclasses" $
            $(inspectTest $ hasNoTypeClasses 'getSyntaxViaLens) `shouldSatisfy` isSuccess

matchMarkPrism :: Grade -> Maybe Int
matchMarkPrism = preview _Mark

matchMarkManual :: Grade -> Maybe Int
matchMarkManual grade = case grade of
    Mark n -> Just n
    _other -> Nothing

prismSpecs :: Spec
prismSpecs = describe "Prism" $ do
    describe "preview" $ do
        it "preview _Ctor x ≡ case (Ctor _) of" $
            $(inspectTest $ 'matchMarkPrism === 'matchMarkManual) `shouldSatisfy` isSuccess

-- Helper functions

isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False
