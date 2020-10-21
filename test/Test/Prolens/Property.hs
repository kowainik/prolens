module Test.Prolens.Property
    ( lensPropertySpecs
    , typeclassesPropertySpecs
    ) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (PropertyT, forAll, forAllWith, hedgehog, (===))

import Prolens
import Test.Data (genFun, genFunction, genHaskeller, genInt, genName, nameL)


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
typeclassesPropertySpecs = describe "Class Laws" $ do
    profunctorsSpec
    monoidalSpec

profunctorsSpec :: Spec
profunctorsSpec = describe "Profunctor" $ do
    describe "(->)" $ do
        it "Identity: dimap id id ≡ id" $ hedgehog $ do
            f <- forAllWith (const "f") genFunction
            x <- forAll genInt
            dimap id id f x === f x
        it "Composition: dimap (ab . bc) (yz . xy) ≡ dimap bc yz . dimap ab xy" $ hedgehog $ do

            f  <- forAllWith (const "f")  genFunction
            ab <- forAllWith (const "ab") genFunction
            bc <- forAllWith (const "bc") genFunction
            xy <- forAllWith (const "xy") genFunction
            yz <- forAllWith (const "xy") genFunction

            n <- forAll genInt
            dimap (ab . bc) (yz . xy) f n === (dimap bc yz . dimap ab xy) f n
    describe "Fun" $ do
        it "Identity: dimap id id ≡ id" $ hedgehog $ do
            f <- forAllWith (const "f") genFun
            eqFun (dimap id id f) f
        it "Composition: dimap (ab . bc) (yz . xy) ≡ dimap bc yz . dimap ab xy" $ hedgehog $ do

            f  <- forAllWith (const "f")  genFun
            ab <- forAllWith (const "ab") genFunction
            bc <- forAllWith (const "bc") genFunction
            xy <- forAllWith (const "xy") genFunction
            yz <- forAllWith (const "xy") genFunction

            eqFun
                (dimap (ab . bc) (yz . xy) f)
                (dimap bc yz $ dimap ab xy f)

eqFun :: Fun Maybe Int Int -> Fun Maybe Int Int -> PropertyT IO ()
eqFun fun1 fun2 = do
    x <- forAll genInt
    unFun fun1 x === unFun fun2 x

monoidalSpec :: Spec
monoidalSpec = describe "Monoidal" $ do
    describe "(->)" $ do
        it "Identity: pappend f pempty ≡ first f" $ hedgehog $ do
            f <- forAllWith (const "f") genFunction
            x <- forAll genInt
            y <- forAll genInt
            pappend f pempty (x, y) === first f (x, y)
        it "Identity: pappend pempty f ≡ second f" $ hedgehog $ do
            f <- forAllWith (const "f") genFunction
            x <- forAll genInt
            y <- forAll genInt
            pappend pempty f (x, y) === second f (x, y)
        it "Associativity (right)" $ hedgehog $ do
            f <- forAllWith (const "f") genFunction
            g <- forAllWith (const "g") genFunction
            h <- forAllWith (const "h") genFunction
            x <- forAll genInt
            y <- forAll genInt
            z <- forAll genInt
            pappend f (pappend g h) (x, (y, z)) === (f x, (g y, h z))
        it "Associativity (left)" $ hedgehog $ do
            f <- forAllWith (const "f") genFunction
            g <- forAllWith (const "g") genFunction
            h <- forAllWith (const "h") genFunction
            x <- forAll genInt
            y <- forAll genInt
            z <- forAll genInt
            pappend (pappend f g) h ((x, y), z) === ((f x, g y), h z)
