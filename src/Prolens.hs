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

      -- * Prisms
      -- ** Prism types
    , Prism
    , Prism'

      -- ** Prism functions
    , prism
    , prism'
    , preview
    ) where


import Control.Applicative (Const (..))
import Data.Coerce (coerce)
import Data.Monoid (First (..))


{- |

Instances of 'Profunctor' should satisfy the following laws:

* __Identity:__ @'dimap' 'id' 'id' ≡ 'id'@
* __Composition:__ @'dimap' (inAB . inBC) (outYZ . outXY) ≡ 'dimap' outBC outYZ . 'dimap' outAB outXY@

@since 0.0.0.0
-}
-- type Profunctor :: (Type -> Type -> Type) -> Constraint
class (forall a . Functor (p a)) => Profunctor p where
    dimap :: (in2 -> in1) -> (out1 -> out2) -> p in1 out1 -> p in2 out2

-- | @since 0.0.0.0
instance Profunctor (->) where
    dimap :: (in2 -> in1) -> (out1 -> out2) -> (in1 -> out1) -> (in2 -> out2)
    dimap in21 out12 f = out12 . f . in21
    {-# INLINE dimap #-}

-- | @since 0.0.0.0
newtype Fun m a b = Fun
    { unFun :: a -> m b
    }

-- | @since 0.0.0.0
instance Functor m => Functor (Fun m x) where
    fmap :: (a -> b) -> Fun m x a -> Fun m x b
    fmap f (Fun xma) = Fun (fmap f . xma)
    {-# INLINE fmap #-}

-- | @since 0.0.0.0
instance Functor m => Profunctor (Fun m) where
    dimap :: (in2 -> in1) -> (out1 -> out2) -> Fun m in1 out1 -> Fun m in2 out2
    dimap in21 out12 (Fun f) = Fun (fmap out12 . f . in21)
    {-# INLINE dimap #-}

{-
type Lens  s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s   a   = forall f. Functor f => (a -> f a) -> s -> f s

-}
class Profunctor p => Strong p where
    first  :: p a b -> p (a, c) (b, c)
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

instance (Applicative m) => Choice (Fun m) where
    left :: Fun m a b -> Fun m (Either a c) (Either b c)
    left (Fun amb)= Fun $ \eitherAc -> case eitherAc of
        Left a  -> Left <$> amb a
        Right c -> pure $ Right c

    right :: Fun m a b -> Fun m (Either c a) (Either c b)
    right (Fun amb)= Fun $ \eitherCa -> case eitherCa of
        Right a -> Right <$> amb a
        Left c  -> pure $ Left c

class Strong p => Monoidal p where
    pappend :: p a b -> p c d -> p (a,c) (b,d)
    pempty :: p a a

instance Monoidal (->) where
    pappend :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
    pappend ab cd (a, c) = (ab a, cd c)

    pempty :: a -> a
    pempty = id

{- | 'Optic' takes a connection from @a@ to @b@ (represented as a
value of type @p a b@) and returns a connection from @source@ to
@target@ (represented as a value of type @p source target@).

@since 0.0.0.0
-}
type Optic p source target a b = p a b -> p source target

type Lens  source target a b = forall p . Strong p => Optic p source target a b
type Lens' source        a   = Lens source source a a

set :: (p ~ (->))
    => Optic p source target a b -> b -> source -> target
set abst = abst . const
{-# INLINE set #-}

over
    :: (p ~ (->))
    => Optic p source target a b -> (a -> b) -> source -> target
over = id
{-# INLINE over #-}

view
    :: (p ~ Fun (Const a))
    => Optic p source target a b -> (source -> a)
view opt = coerce (opt (Fun Const))
{-# INLINE view #-}
-- view opt = getConst . unFun (opt (Fun Const))
    -- opt :: Fun (Const a) a b -> Fun (Const a) s t
    -- opt :: (a -> Const a b) -> ( s -> Const a t)

lens :: (source -> a) -> (source -> b -> target) -> Lens source target a b
lens sa sbt = dimap (\s -> (s, sa s)) (uncurry sbt) . second
{-# INLINE lens #-}


{- | The operator form of 'view' with the arguments flipped.

@since 0.0.0.0
-}
infixl 8 ^.
(^.) :: source -> Lens' source a -> a
s ^. l = view l s
{-# INLINE (^.) #-}

{- | The operator form of 'set'.

@since 0.0.0.0
-}
infixr 4 .~
(.~) :: Lens' source a -> a -> source -> source
(.~) = set
{-# INLINE (.~) #-}

{- | The operator form of 'over'.

@since 0.0.0.0
-}
infixr 4 %~
(%~) :: Lens' source a -> (a -> a) -> source -> source
(%~) = over
{-# INLINE (%~) #-}

-- Prisms

type Prism  source target a b = forall p . Choice p => Optic p source target a b
type Prism' source        a   = Prism source source a a

newtype Forget r a b = Forget
    { unForget :: a -> r
    }

instance Functor (Forget r x) where
    fmap :: (a -> b) -> Forget r x a -> Forget r x b
    fmap _ = coerce

instance Profunctor (Forget r) where
    dimap :: (a -> b) -> (c -> d) -> Forget r b c -> Forget r a d
    dimap ab _cd (Forget br) = Forget (br . ab)

instance Monoid r => Choice (Forget r) where
    left :: Forget r a b -> Forget r (Either a c) (Either b c)
    left (Forget ar) = Forget (either ar (const mempty))

    right :: Forget r a b -> Forget r (Either c a) (Either c b)
    right (Forget ar) = Forget (either (const mempty) ar)

preview
    :: (p ~ Forget (First a))
    => Optic p source source a a -> source -> Maybe a
preview paapss = getFirst . unForget (paapss (Forget (First . Just)))
-- paapss :: Forget (First a) a a -> Forget (First a) source source
-- paapss :: (a -> First a) -> source -> First a
-- paapss :: (a -> Maybe a) -> source -> Maybe a

prism :: (b -> target) -> (source -> Either target a) -> Prism source target a b
-- prism :: (b -> target) -> (source -> Either target a) -> p a b -> p source target
prism bTarget sEitherTargetA pab = dimap sEitherTargetA targetBtarget $ right pab
  where
    targetBtarget = either id bTarget

prism' :: (a -> source) -> (source -> Maybe a) -> Prism' source a
prism' aSource sourceMaybeA = prism aSource (\s -> maybe (Left s) Right $ sourceMaybeA s)
