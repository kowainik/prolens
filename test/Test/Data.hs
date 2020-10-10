module Test.Data
    ( Haskeller (..)
    , Knowledge (..)
    , nameL

    , me
    ) where

import Prolens (Lens', lens)


data Haskeller = Haskeller
    { haskellerName       :: String
    , haskellerExperience :: Int
    , haskellerKnowledge  :: Knowledge
    } deriving stock (Show)

data Knowledge = Knowledge
    { kSyntax         :: Bool
    , kMonads         :: Bool
    , kLens           :: Bool
    , kTypeLevelMagic :: Bool
    , kNix            :: Bool
    } deriving stock (Show)

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
nameL = lens getter setter
  where
    getter :: Haskeller -> String
    getter = haskellerName

    setter :: String -> Haskeller -> Haskeller
    setter newName h = h { haskellerName = newName }
