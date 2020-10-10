module Test.Data
    ( Haskeller (..)
    , nameL
    , knowledgeL

    , Knowledge (..)
    , syntaxL

    , me

      -- * Generators
    , genHaskeller
    , genKnowledge
    , genName
    ) where

import Test.Hspec.Hedgehog (MonadGen)

import Prolens (Lens', lens)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


data Haskeller = Haskeller
    { haskellerName       :: String
    , haskellerExperience :: Int
    , haskellerKnowledge  :: Knowledge
    } deriving stock (Show, Eq)

data Knowledge = Knowledge
    { kSyntax         :: Bool
    , kMonads         :: Bool
    , kLens           :: Bool
    , kTypeLevelMagic :: Bool
    , kNix            :: Bool
    } deriving stock (Show, Eq)

me :: Haskeller
me = Haskeller
    { haskellerName = "Veronika"
    , haskellerExperience = 2
    , haskellerKnowledge = Knowledge
        { kSyntax = True
        , kMonads = True
        , kLens = True
        , kTypeLevelMagic = True
        , kNix = False
        }
    }

nameL :: Lens' Haskeller String
nameL = lens haskellerName (\h new -> h { haskellerName = new })
{-# INLINE nameL #-}

knowledgeL :: Lens' Haskeller Knowledge
knowledgeL = lens haskellerKnowledge (\h new -> h { haskellerKnowledge = new })
{-# INLINE knowledgeL #-}

syntaxL :: Lens' Knowledge Bool
syntaxL = lens kSyntax (\k new -> k { kSyntax = new })
{-# INLINE syntaxL #-}

-- Generators

genKnowledge :: (MonadGen m) => m Knowledge
genKnowledge = do
    kSyntax <- Gen.bool
    kMonads <- Gen.bool
    kLens <- Gen.bool
    kTypeLevelMagic <- Gen.bool
    kNix <- Gen.bool
    pure Knowledge{..}

genHaskeller :: (MonadGen m) => m Haskeller
genHaskeller = do
    haskellerName <- genName
    haskellerExperience <- Gen.int $ Range.linear 0 50
    haskellerKnowledge <- genKnowledge
    pure Haskeller{..}

genName :: MonadGen m => m String
genName = Gen.string (Range.linear 1 50) Gen.alphaNum
