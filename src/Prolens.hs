{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Profunctor based lightweight implementation of Lenses
-}

module Prolens
    ( -- * Typeclasses and data types
      Profunctor (..)
    , Strong (..)
    , Choice (..)
    , Monoidal (..)
    , Fun (..)

      -- * Lenses types
    , Optic
    , Lens
    , Lens'

      -- * Lenses functions
    , set
    , over
    , view
    , lens

      -- * Lenses operators
    , (^.)
    , (.~)
    , (%~)
    ) where


import Control.Applicative (Const (..))
import Data.Coerce (coerce)


-- type Profunctor :: (Type -> Type -> Type) -> Constraint
class (forall a . Functor (p a)) => Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
    dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
    dimap ab cd bc = cd . bc . ab
    {-# INLINE dimap #-}

newtype Fun m a b = Fun
    { unFun :: a -> m b
    }

-- @since 0.0.0.0
instance Functor m => Functor (Fun m x) where
    fmap :: (a -> b) -> Fun m x a -> Fun m x b
    fmap f (Fun xma) = Fun (fmap f . xma)
    {-# INLINE fmap #-}

instance Functor m => Profunctor (Fun m) where
    dimap :: (a -> b) -> (c -> d) -> Fun m b c -> Fun m a d
    dimap ab cd (Fun bmc) = Fun (fmap cd . bmc . ab)
    {-# INLINE dimap #-}

{-
type Lens  s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s   a   = forall f. Functor f => (a -> f a) -> s -> f s

-}
class Profunctor p => Strong p where
    first :: p a b -> p (a, c) (b, c)
    second :: p a b -> p (c, a) (c, b)

instance Strong (->) where
    first :: (a -> b) -> (a, c) -> (b, c)
    first ab (a, c) = (ab a, c)
    {-# INLINE first #-}

    second :: (a -> b) -> (c, a) -> (c, b)
    second ab (c, a) = (c, ab a)
    {-# INLINE second #-}

instance (Functor m) => Strong (Fun m) where
    first :: Fun m a b -> Fun m (a, c) (b, c)
    first (Fun amb) = Fun (\(a, c) -> fmap (, c) (amb a))
    {-# INLINE first #-}

    second :: Fun m a b -> Fun m (c, a) (c, b)
    second (Fun amb) = Fun (\(c, a) -> fmap (c,) (amb a))
    {-# INLINE second #-}

class Profunctor p => Choice p where
    left :: p a b -> p (Either a c) (Either b c)
    right :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
    left  :: (a -> b) -> Either a c -> Either b c
    left ab = \case
        Left a  -> Left $ ab a
        Right c -> Right c

    right :: (a -> b) -> Either c a -> Either c b
    right ab = \case
        Right a -> Right $ ab a
        Left c  -> Left c

class Strong p => Monoidal p where
    pappend :: p a b -> p c d -> p (a,c) (b,d)
    pempty :: p a a

instance Monoidal (->) where
    pappend :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
    pappend ab cd (a, c) = (ab a, cd c)

    pempty :: a -> a
    pempty = id

type Optic p s t a b = p a b -> p s t

type Lens  s t a b = forall p . Strong p => Optic p s t a b
type Lens' s   a   = Lens s s a a

set :: (p ~ (->))
    => Optic p s t a b -> b -> s -> t
set abst = abst . const
{-# INLINE set #-}

over
    :: (p ~ (->))
    => Optic p s t a b -> (a -> b) -> s -> t
over = id
{-# INLINE over #-}

view
    :: (p ~ Fun (Const a))
    => Optic p s t a b -> (s -> a)
view opt = coerce (opt (Fun Const))
{-# INLINE view #-}
-- view opt = getConst . unFun (opt (Fun Const))
    -- opt :: Fun (Const a) a b -> Fun (Const a) s t
    -- opt :: (a -> Const a b) -> ( s -> Const a t)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt = dimap (\s -> (s, sa s)) (uncurry sbt) . second
{-# INLINE lens #-}


{- | The operator form of 'view' with the arguments flipped.

@since 0.0.0.0
-}
infixl 8 ^.
(^.) :: s -> Lens' s a -> a
s ^. l = view l s
{-# INLINE (^.) #-}

{- | The operator form of 'set'.

@since 0.0.0.0
-}
infixr 4 .~
(.~) :: Lens' s a -> a -> s -> s
(.~) = set
{-# INLINE (.~) #-}

{- | The operator form of 'over'.

@since 0.0.0.0
-}
infixr 4 %~
(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over
{-# INLINE (%~) #-}
