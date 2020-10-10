module Test.Data
    ( Haskeller (..)
    , nameL
    , knowledgeL

    , Knowledge (..)
    , syntaxL

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
nameL = lens haskellerName (\h new -> h { haskellerName = new })
{-# INLINE nameL #-}

knowledgeL :: Lens' Haskeller Knowledge
knowledgeL = lens haskellerKnowledge (\h new -> h { haskellerKnowledge = new })
{-# INLINE knowledgeL #-}

syntaxL :: Lens' Knowledge Bool
syntaxL = lens kSyntax (\k new -> k { kSyntax = new })
{-# INLINE syntaxL #-}
