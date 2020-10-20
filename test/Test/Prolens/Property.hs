module Test.Prolens.Property
    ( lensPropertySpecs
    , typeclassesPropertySpecs
    ) where

import Hedgehog (Gen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (PropertyT, forAll, forAllWith, hedgehog, (===))

import Prolens
import Test.Data (genFun, genFunction, genForget, genHaskeller, genInt, genName, nameL)


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

typeclassesPropertySpecs :: Spec
typeclassesPropertySpecs = describe "Class Laws" -- $ do
    profunctorsSpec

profunctorsSpec :: Spec
profunctorsSpec = describe "Profunctor" $ do
    profunctorLaws "(->)" genFunction eqFunction
    profunctorLaws "Fun" genFun eqFun
    profunctorLaws "Forget" genForget eqForget


profunctorLaws
  :: Profunctor p
  => String
  -> Gen (p Int Int)
  -> (p Int Int -> p Int Int -> PropertyT IO ())
  -> Spec
profunctorLaws name genProfunctor cmp =
  describe name $ do
    it "Identity: dimap id id ≡ id" $ hedgehog $ do
        f <- forAllWith (const "f") genProfunctor
        dimap id id f `cmp` f

    it "Composition: dimap (ab . bc) (yz . xy) ≡ dimap bc yz . dimap ab xy" $ hedgehog $ do
        f  <- forAllWith (const "f")  genProfunctor
        ab <- forAllWith (const "ab") genFunction
        bc <- forAllWith (const "bc") genFunction
        xy <- forAllWith (const "xy") genFunction
        yz <- forAllWith (const "xy") genFunction

        dimap (ab . bc) (yz . xy) f `cmp` (dimap bc yz . dimap ab xy) f


eqFunction :: (Int -> Int) -> (Int -> Int) -> PropertyT IO ()
eqFunction f g = do
    n <- forAll genInt
    f n === g n

eqFun :: Fun Maybe Int Int -> Fun Maybe Int Int -> PropertyT IO ()
eqFun fun1 fun2 = do
    x <- forAll genInt
    unFun fun1 x === unFun fun2 x

eqForget :: Forget Int Int a -> Forget Int Int a -> PropertyT IO ()
eqForget forget1 forget2 = do
    x <- forAll genInt
    unForget forget1 x === unForget forget2 x
