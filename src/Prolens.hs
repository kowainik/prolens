{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Profunctor based lightweight implementation of Lenses

The @prolens@ package is a Haskell library with a minimal and lightweight
implementation of optics based on 'Profunctor's. __Optic__ is a high-level
concept for values providing composable access to different parts of structures.

"Prolens" provides the following optics:

 * 'Lens' — composable getters and setters
 * 'Prisms' — composable constructors and deconstructors

== Usage

To use lenses or prisms in your project, you need to add @prolens@ package as
the dependency in the @build-depends@ field of your @.cabal@ file. E.g.:

@
build-depends: prolens ^>= 0.0.0.0
@

You should add the import of this module in the place of lenses usage:

@
__import__ Prolens
@


-}

module Prolens
    ( -- * Profunctors
      -- $profunctors
      -- ** Typeclasses and data types
      Profunctor (..)
    , Strong (..)
    , Choice (..)
    , Monoidal (..)
    , Fun (..)

      -- * Optics
    , Optic

      -- * Lenses
      -- $lenses
      -- ** Lenses types
    , Lens
    , Lens'

      -- ** Lenses functions
    , set
    , over
    , view
    , lens

      -- ** Lenses operators
    , (^.)
    , (.~)
    , (%~)

      -- * Prisms
      -- $prisms
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

-- $setup
-- >>> import Data.Function ((&))


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


{- $lenses

== Example

To understand better how to use this library lets look at some simple example.
Let's say we have the user and address data types in our system:

>>> :{
data Address = Address
    { addressCountry :: String
    , addressCity    :: String
    , addressIndex   :: String
    } deriving (Show)
:}

>>> :{
data User = User
    { userName    :: String
    , userAge     :: Int
    , userAddress :: Address
    } deriving (Show)
:}

We can easily get fields of the @User@ and @Address@ types, but setting values is inconvenient (especially for nested records). To solve this problem, we can use lenses — composable getters and setters. 'Lens' is a value, so we need to define lenses for fields of our data types first.

To create the lens for the @userName@ field we can use 'lens' function and
manually writing getter and setter function:

>>> :{
nameL :: Lens' User String
nameL = lens getter setter
  where
    getter :: User -> String
    getter = userName
    setter :: User -> String -> User
    setter user newName = user {userName = newName}
:}

In this manner, we can create other lenses for our @User@ data type.
Usually, lenses are one-liners, and we can define them easily using lambda-functions.

>>> :{
ageL :: Lens' User Int
ageL = lens userAge (\u new -> u {userAge = new})
:}

>>> :{
addressL :: Lens' User Address
addressL = lens userAddress (\u new -> u {userAddress = new})
:}

We want to have lenses for accessing @Adress@ fields inside @User@, so we want to have the following values:

@
countryL :: 'Lens'' User 'String'
cityL    :: 'Lens'' User 'String'
indexL   :: 'Lens'' User 'String'
@

/Note:/ for lenses as @countryL@, @cityL@ etc., we are using composition of the
lenses for the @userAddress@ field. If we have

>>> :{
addressCityL :: Lens' Address String
addressCityL = lens addressCity (\a new -> a {addressCity = new})
:}

then

>>> cityL = addressL . addressCityL

Let's say we have some sample user

>>> :{
address = Address
    { addressCountry = "UK"
    , addressCity    = "London"
    , addressIndex   = "XXX"
    }
user :: User
user = User
    { userName = "John"
    , userAge  = 42
    , userAddress = address
    }
:}

To view the fields of the User data type we can use 'view' or '^.'

>>> view ageL user
42
>>> user ^. cityL
"London"

If we want to change any of the user's data, we should use 'set' or '.~'

>>> set nameL "Johnny" user
User {userName = "Johnny", userAge = 42, userAddress = Address {addressCountry = "UK", addressCity = "London", addressIndex = "XXX"}}
>>> user & cityL .~ "Bristol"
User {userName = "John", userAge = 42, userAddress = Address {addressCountry = "UK", addressCity = "Bristol", addressIndex = "XXX"}}

'over' or '%~' operator could be useful when, for example, you want to increase the age by one on the user's birthday:

>>> over ageL succ user
User {userName = "John", userAge = 43, userAddress = Address {addressCountry = "UK", addressCity = "London", addressIndex = "XXX"}}
>>> user & ageL %~ succ
User {userName = "John", userAge = 43, userAddress = Address {addressCountry = "UK", addressCity = "London", addressIndex = "XXX"}}
-}

{- | Lenses

@since 0.0.0.0
-}
type Lens  source target a b = forall p . Strong p => Optic p source target a b

{- | The monomorphic lenses which don't change the type of the container (or of
the value inside). It has a 'Strong' constraint, and it can be used whenever a
getter or a setter is needed.

  * @a@ is the type of the value inside of structure
  * @source@ is the type of the whole structure

@since 0.0.0.0
-}
type Lens' source        a   = Lens source source a a

{- | Sets the given value to the structure using a setter.

@since 0.0.0.0
-}
set :: (p ~ (->))
    => Optic p source target a b -> b -> source -> target
set abst = abst . const
{-# INLINE set #-}

{- | Applies the given function to the target.

@since 0.0.0.0
-}
over
    :: (p ~ (->))
    => Optic p source target a b -> (a -> b) -> source -> target
over = id
{-# INLINE over #-}

{- | Gets a value out of a structure using a getter.

@since 0.0.0.0
-}
view
    :: (p ~ Fun (Const a))
    => Optic p source target a b -> (source -> a)
view opt = coerce (opt (Fun Const))
{-# INLINE view #-}
-- view opt = getConst . unFun (opt (Fun Const))
    -- opt :: Fun (Const a) a b -> Fun (Const a) s t
    -- opt :: (a -> Const a b) -> ( s -> Const a t)

{- | Creates 'Lens' from the getter and setter.

@since 0.0.0.0
-}
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

{- $prisms
Prisms work with sum types.

== Example

Let's say we have the user data type in our system:

>>> :{
data Address = Address
    { addressCountry :: String
    , addressCity    :: String
    } deriving (Show)
:}

>>> :{
data Payload
    = NamePayload String
    | IdPayload Int
    | AddressPayload Address
    deriving (Show)
:}

To create the prism for each constructor we can use 'prism'' function and
manually writing getter and setter function:

/NOTE:/ The naming convention for prisms is the following:

@
_ConstructorName
@

>>> :{
_NamePayload :: Prism' Payload String
_NamePayload = prism' construct match
  where
    match :: Payload -> Maybe String
    match p = case p of
        NamePayload name -> Just name
        _otherPayload -> Nothing
    construct :: String -> Payload
    construct = NamePayload
:}

In this manner, we can create other prisms for our @Payload@ data type.

>>> :{
_IdPayload :: Prism' Payload Int
_IdPayload = prism' IdPayload $ \p -> case p of
    IdPayload i -> Just i
    _otherPayload -> Nothing
:}

>>> :{
_AddressPayload :: Prism' Payload Address
_AddressPayload = prism' AddressPayload $ \p -> case p of
    AddressPayload a -> Just a
    _otherPayload -> Nothing
:}

Let's say we have some sample payload

>>> :{
payloadName :: Payload
payloadName = NamePayload "Some name"
:}

To view the fields of the @Payload@ data type we can use 'preview'

>>> preview _NamePayload payloadName
Just "Some name"
>>> preview _IdPayload payloadName
Nothing

If we want to change any of the data, we should use 'set' or '.~' (just like in lenses)

>>> set _NamePayload "Johnny" payloadName
NamePayload "Johnny"
>>> set _IdPayload 3 payloadName
NamePayload "Some name"

Note, that you can easily compose lenses and prisms together:

>>> :{
address = Address
    { addressCountry = "UK"
    , addressCity    = "London"
    }
:}

>>> :{
addressCityL :: Lens' Address String
addressCityL = lens addressCity (\a new -> a {addressCity = new})
:}

>>> :{
payloadAddress :: Payload
payloadAddress = AddressPayload address
:}

>>> set _AddressPayload (address & addressCityL .~ "Bristol") payloadAddress
AddressPayload (Address {addressCountry = "UK", addressCity = "Bristol"})
-}

{- |

@since 0.0.0.0
-}
type Prism  source target a b = forall p . Choice p => Optic p source target a b

{- |

@since 0.0.0.0
-}
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

{- |

@since 0.0.0.0
-}
preview
    :: (p ~ Forget (First a))
    => Optic p source source a a -> source -> Maybe a
preview paapss = getFirst . unForget (paapss (Forget (First . Just)))
-- paapss :: Forget (First a) a a -> Forget (First a) source source
-- paapss :: (a -> First a) -> source -> First a
-- paapss :: (a -> Maybe a) -> source -> Maybe a

{- |

@since 0.0.0.0
-}
prism :: (b -> target) -> (source -> Either target a) -> Prism source target a b
-- prism :: (b -> target) -> (source -> Either target a) -> p a b -> p source target
prism bTarget sEitherTargetA pab = dimap sEitherTargetA targetBtarget $ right pab
  where
    targetBtarget = either id bTarget

{- |

@since 0.0.0.0
-}
prism' :: (a -> source) -> (source -> Maybe a) -> Prism' source a
prism' aSource sourceMaybeA = prism aSource (\s -> maybe (Left s) Right $ sourceMaybeA s)
