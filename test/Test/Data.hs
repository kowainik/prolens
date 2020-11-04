module Test.Data
    ( -- * Lenses
      Haskeller (..)
    , nameL
    , knowledgeL

    , Knowledge (..)
    , syntaxL

    , me

      -- * Prisms
    , Grade (..)
    , _Mark

    , gradeMark
    , gradeOther

      -- * Generators
    , genFun
    , genFunction
    , genForget
    , genHaskeller
    , genInt
    , genKnowledge
    , genName
    ) where

import Test.Hspec.Hedgehog (MonadGen)
import GHC.Exts (inline)

import Prolens (Forget (..), Fun (..), Lens', Prism', lens, prism')

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

data Grade
    = Mark Int
    | Other String
    deriving stock (Show, Eq)

_Mark :: Prism' Grade Int
_Mark = inline prism' Mark $ \case
    Mark a -> Just a
    _other -> Nothing
{-# INLINE _Mark #-}

gradeMark :: Grade
gradeMark = Mark 5

gradeOther :: Grade
gradeOther = Other "Satisfactory"

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

genInt :: MonadGen m => m Int
genInt = Gen.enumBounded

genFunction :: MonadGen m => m (Int -> Int)
genFunction = genInt >>= \n -> Gen.element
    [ id
    , const n
    , (+ n)
    , (* n)
    , subtract n
    , \x -> if x >= n then 1 else 0
    ]

genFun :: MonadGen m => m (Fun Maybe Int Int)
genFun = genFunction >>= \f -> Gen.element $ map Fun
    [ Just
    , const Nothing
    , Just . f
    ]

genForget :: MonadGen m => m (Forget Int Int a)
genForget = Forget . unFun <$> genFun
