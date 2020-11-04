{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

The @prolens@ package is a Haskell library with a minimal and lightweight
implementation of optics based on 'Profunctor's. __'Optic'__ is a high-level
concept for values that provide composable access to different parts of structures.

"Prolens" implements the following optics:

 * 'Lens' — composable getters and setters
 * 'Prism' — composable constructors and deconstructors
 * 'Traversal' — composable data structures visitors

== Usage

To use lenses or prisms in your project, you need to add @prolens@ package as
the dependency in the @build-depends@ field of your @.cabal@ file. E.g.:

@
build-depends: prolens ^>= 0.0.0.0
@

You should add the import of this module in the place of lenses usage:

@
__import__ "Prolens"
@

== Creating your own optics

We show in each section of this module how to create values of each
kind of optics.

⚠️ __The general crucial rule__ for achieving maximum performance:
always add @\{\-\# INLINE ... \#\-\}@ pragmas to your optics.

== Typeclasses table

The below table shows required constraints for each 'Optic':

+-------------+------------------------------+
| Optic       | Constraints                  |
+=============+==============================+
| 'Lens'      | @'Strong' p@                 |
+-------------+------------------------------+
| 'Prism'     | @'Choice' p@                 |
+-------------+------------------------------+
| 'Traversal' | @('Choice' p, 'Monoidal' p)@ |
+-------------+------------------------------+

== Usage table: get, set, modify

Here is a go-to table on how to use getter, setters and modifiers with different
'Optic's.

+-------------+------------------+--------------+------------------+------------------+-----------------+-----------------+
|             | get              | get operator | set              | set operator     | modify          | modify operator |
+=============+==================+==============+==================+==================+=================+=================+
| 'Lens'      | @'view' l x@     | @x '^.' l@   | @'set' l new x@  | @x & l '.~' new@ | @'over' l f x@  | @x & l '%~' f@  |
+-------------+------------------+--------------+------------------+------------------+-----------------+-----------------+
| 'Prism'     | @'preview' _L x@ | -            | @'set' _L new x@ | -                | @'over' _L f x@ | -               |
+-------------+------------------+--------------+------------------+------------------+-----------------+-----------------+
| 'Traversal' | @'view' l x@     | -            | @'set' l new x@  | -                | @'over' l f x@  | -               |
+-------------+------------------+--------------+------------------+------------------+-----------------+-----------------+

@since 0.0.0.0
-}

module Prolens
    ( -- * Profunctor typeclass
      Profunctor (..)

      -- * Optics
    , Optic

      -- * Lenses
      -- $lenses

      -- ** Lenses types
    , Lens
    , Lens'
      -- ** Strong typeclass
    , Strong (..)

      -- ** Lenses functions
    , set
    , over
    , view
    , lens

      -- ** Lenses operators
    , (^.)
    , (.~)
    , (%~)

      -- ** Standard lenses
    , fstL
    , sndL

      -- * Prisms
      -- $prisms

      -- ** Prism types
    , Prism
    , Prism'
      -- ** Choice typeclass
    , Choice (..)

      -- ** Prism functions
    , prism
    , prism'
    , preview

      -- ** Standard Prisms
    , _Just
    , _Left
    , _Right

      -- * Traversals

      -- ** Traversal types
    , Traversal
      -- ** Monoidal typeclass
    , Monoidal (..)

      -- ** Traversal functions
    , traverseOf

      -- ** Standard traversals
    , eachPair
    , eachMaybe
    , eachList

      -- * Internal data types
    , Forget (..)
    , Fun (..)
    ) where

import Control.Applicative (Const (..), liftA2)
import Data.Coerce (coerce)
import Data.Monoid (First (..))


-- $setup
-- >>> import Data.Function ((&))


{- | The type @p@ is called 'Profunctor' and it means, that a value of
type @p in out@ takes a value of type @in@ as an argument (input) and
outputs a value of type @out@. 'Profunctor' allows mapping of inputs
and outputs.

@
          +----> Result input
          |
          |                                +--> Original profunctor
          |      +-> Original input        |
          +      +                         +
dimap :: (in2 -> in1) -> (out1 -> out2) -> p in1 out1 -> p in2 out2
                          +       +
                          |       +-> Result output
                          |
                          +-> Original output
@

Speaking in terms of other abstractions, 'Profunctor' is
'Data.Functor.Contravariant.Contravariant' in the first type argument
(type variable @in@) and 'Functor' in the second type argument (type
variable @out@).

Moreover, @p in@ must have 'Functor' instance first to implement the
'Profunctor' instance. This required using @QuantifiedConstraints@.

@
                         Contravariant <---+
                                           |
                                         +-+-+
                                         +   +
(forall a . Functor (p a)) => Profunctor p a b
          +                              + +
          |                              | |
          +--> Quantified constraint     +++
                                          |
                              Functor  <--+
@

Instances of 'Profunctor' should satisfy the following laws:

* __Identity:__ @'dimap' 'id' 'id' ≡ 'id'@
* __Composition:__ @'dimap' (inAB . inBC) (outYZ . outXY) ≡ 'dimap' inBC outYZ . 'dimap' inAB outXY@

@since 0.0.0.0
-}
-- type Profunctor :: (Type -> Type -> Type) -> Constraint
class (forall a . Functor (p a)) => Profunctor p where
    dimap
        :: (in2 -> in1)  -- ^ Map input
        -> (out1 -> out2)  -- ^ Map output
        -> p in1 out1  -- ^ Take @in1@ as input and return @out1@
        -> p in2 out2  -- ^ Take @in2@ as input and return @out2@

-- | @since 0.0.0.0
instance Profunctor (->) where
    dimap :: (in2 -> in1) -> (out1 -> out2) -> (in1 -> out1) -> (in2 -> out2)
    dimap in21 out12 f = out12 . f . in21
    {-# INLINE dimap #-}

{- | @'Fun' m a b@ is a wrapper for function @a -> m b@.

@since 0.0.0.0
-}
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

{- | 'Strong' is a 'Profunctor' that can be lifted to take a pair as
an input and return a pair.

The second element of a pair (variable of type @c@) can be of any
type, and you can decide what type it should be. This is convenient
for implementing various functions. E.g. 'lens' uses this fact.

Instances of 'Strong' should satisfy the following laws:

* __'first' via 'second' swap:__ @'first' ≡ 'dimap' 'Data.Tuple.swap' 'Data.Tuple.swap' . 'second'@
* __'second' via 'first' swap:__ @'second' ≡ 'dimap' 'Data.Tuple.swap' 'Data.Tuple.swap' . 'first'@

* __Fst functor:__ @'dimap' 'fst' 'id' ≡ 'fmap' 'fst' . 'first'@
* __Snd functor:__ @'dimap' 'snd' 'id' ≡ 'fmap' 'snd' . 'second'@

* __Distribution over 'first':__ @'dimap' ('second' f) 'id' . 'first' ≡ 'fmap' ('second' f) . 'first'@
* __Distribution over 'second':__ @'dimap' ('first' f) 'id' . 'second' ≡ 'fmap' ('first' f) . 'second'@

* __Associativity of 'first':__ @'first' . 'first' ≡ 'dimap' (\\((a, b), c) -> (a, (b, c))) (\\(a, (b, c)) -> ((a, b), c)) . 'first'@
* __Associativity of 'second':__ @'second' . 'second' ≡ 'dimap' (\\(a, (b, c)) -> ((a, b), c)) (\\((a, b), c) -> (a, (b, c))) . 'second'@

@since 0.0.0.0
-}
class Profunctor p => Strong p where
    first  :: p a b -> p (a, c) (b, c)
    second :: p a b -> p (c, a) (c, b)

-- | @since 0.0.0.0
instance Strong (->) where
    first :: (a -> b) -> (a, c) -> (b, c)
    first ab (a, c) = (ab a, c)
    {-# INLINE first #-}

    second :: (a -> b) -> (c, a) -> (c, b)
    second ab (c, a) = (c, ab a)
    {-# INLINE second #-}

-- | @since 0.0.0.0
instance (Functor m) => Strong (Fun m) where
    first :: Fun m a b -> Fun m (a, c) (b, c)
    first (Fun amb) = Fun (\(a, c) -> fmap (, c) (amb a))
    {-# INLINE first #-}

    second :: Fun m a b -> Fun m (c, a) (c, b)
    second (Fun amb) = Fun (\(c, a) -> fmap (c,) (amb a))
    {-# INLINE second #-}

{- | 'Choice' is a 'Profunctor' that can be lifted to work with
'Either' given input or some other value.

The other element of 'Either' (variable of type @c@) can be of any
type, and you can decide what type it should be. This is convenient
for implementing various functions. E.g. 'prism' uses this fact.


Assuming, we have the following functions in scope:

@
swapEither  :: Either a b -> Either b a
unnestLeft  :: Either (Either a b) c -> Either a (Either b c)
unnestRight :: Either a (Either b c) -> Either (Either a b) c
@

Instances of 'Choice' should satisfy the following laws:

* __'left' via 'right' swap:__ @'left' ≡ 'dimap' swapEither swapEither . 'right'@
* __'right' via 'left' swap:__ @'right' ≡ 'dimap' swapEither swapEither . 'left'@

* __'Left' functor:__ @'fmap' 'Left' ≡ 'dimap' 'Left' 'id' . 'left'@
* __'Right' functor:__ @'fmap' 'Right' ≡ 'dimap' 'Right' 'id' . 'right'@

* __Distribution over 'left':__ @'dimap' ('right' f) 'id' . 'left' ≡ 'fmap' ('right' f) . 'left'@
* __Distribution over 'right':__ @'dimap' ('left' f) 'id' . 'right' ≡ 'fmap' ('left' f) . 'right'@

* __Associativity of 'left':__ @'left' . 'left' ≡ 'dimap' unnestLeft unnestRight . 'left'@
* __Associativity of 'right':__ @'right' . 'right' ≡ 'dimap' unnestRight unnestLeft . 'right'@

@since 0.0.0.0
-}
class Profunctor p => Choice p where
    left  :: p a b -> p (Either a c) (Either b c)
    right :: p a b -> p (Either c a) (Either c b)

-- | @since 0.0.0.0
instance Choice (->) where
    left  :: (a -> b) -> Either a c -> Either b c
    left ab = \case
        Left a  -> Left $ ab a
        Right c -> Right c
    {-# INLINE left #-}

    right :: (a -> b) -> Either c a -> Either c b
    right ab = \case
        Right a -> Right $ ab a
        Left c  -> Left c
    {-# INLINE right #-}

-- | @since 0.0.0.0
instance (Applicative m) => Choice (Fun m) where
    left :: Fun m a b -> Fun m (Either a c) (Either b c)
    left (Fun amb)= Fun $ \eitherAc -> case eitherAc of
        Left a  -> Left <$> amb a
        Right c -> pure $ Right c
    {-# INLINE left #-}

    right :: Fun m a b -> Fun m (Either c a) (Either c b)
    right (Fun amb)= Fun $ \eitherCa -> case eitherCa of
        Right a -> Right <$> amb a
        Left c  -> pure $ Left c
    {-# INLINE right #-}

{- | 'Monoidal' is 'Strong' 'Profunctor' that can be appended. It is
similar to 'Monoid's but for higher-kinded types.

Instances of 'Monoidal' should satisfy the following laws:

* __Right identity:__ @'pappend' f 'pempty' ≡ 'first' f@
* __Left identity:__ @'pappend' 'pempty' f ≡ 'second' f@
* __Associativity:__ @'pappend' f ('pappend' g h) ⋍ 'pappend' ('pappend' f g) h@

⚠️ __Note:__ The @⋍@ operator in the __associativity__ law is equality
ignoring the structure. The law is written in that way because
'pappend' returns a tuple and the order of nested tuples depends on
the order of 'pappend' applications. In practice, this means, that if
you want to check the law, you reorder tuples in the following way:

@
'pappend' f ('pappend' g h) ≡ 'dimap' (\\(a, (b, c)) -> ((a, b), c)) (\\((a, b), c) -> (a, (b, c))) ('pappend' ('pappend' f g) h)
@

@since 0.0.0.0
-}
class Strong p => Monoidal p where
    pappend :: p a b -> p c d -> p (a, c) (b, d)
    pempty  :: p a a

-- | @since 0.0.0.0
instance Monoidal (->) where
    pappend :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
    pappend ab cd (a, c) = (ab a, cd c)
    {-# INLINE pappend #-}

    pempty :: a -> a
    pempty = id
    {-# INLINE pempty #-}

-- | @since 0.0.0.0
instance (Applicative m) => Monoidal (Fun m) where
    pappend :: Fun m a b -> Fun m c d -> Fun m (a, c) (b, d)
    pappend (Fun amb) (Fun cmd) = Fun (\(a, c) -> liftA2 (,) (amb a) (cmd c))
    {-# INLINE pappend #-}

    pempty :: Fun m a a
    pempty = Fun (pure . id)
    {-# INLINE pempty #-}

{- | 'Optic' takes a connection from @a@ to @b@ (represented as a
value of type @p a b@) and returns a connection from @source@ to
@target@ (represented as a value of type @p source target@).

@
           +---> Profunctor
           |
           | +----> Final input
           | |
           | |      +-> Final output
           | |      |
           + +      +
type Optic p source target a b
                           + +
                           | |
            Given input <--+ |
                             |
        Given output <-------+
@

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

We can easily get fields of the @User@ and @Address@ types, but
setting values is inconvenient (especially for nested records). To
solve this problem, we can use lenses — composable getters and
setters. 'Lens' is a value, so we need to define lenses for fields of
our data types first.

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

{- | 'Lens' represents composable getters and setters.

'Lens' is an @'Optic' p@ with the 'Strong' constraint on the @p@ type variable.

@
          +---> Current object
          |
          |      +-> Final object
          |      |
          +      +
type Lens source target a b
                        + +
                        | |
       Current field <--+ |
                          |
      Final field <-------+
@

@since 0.0.0.0
-}
type Lens source target a b = forall p . Strong p => Optic p source target a b

{- | The monomorphic lenses which don't change the type of the container (or of
the value inside). It has a 'Strong' constraint, and it can be used whenever a
getter or a setter is needed.

  * @a@ is the type of the value inside of structure
  * @source@ is the type of the whole structure

For most use-cases it's enought to use this 'Lens'' instead of more general 'Lens'.

@since 0.0.0.0
-}
type Lens' source a = Lens source source a a

{- | Sets the given value to the structure using a setter.

@since 0.0.0.0
-}
set :: (p ~ (->))
    => Optic p source target a b  -- ^ 'Optic' that can be lens
    -> b  -- ^ Value to set
    -> source  -- ^ Object where we want to set value
    -> target  -- ^ Resulting object with @b@ set
set abst = abst . const
{-# INLINE set #-}

{- | Applies the given function to the target.

@since 0.0.0.0
-}
over
    :: (p ~ (->))
    => Optic p source target a b  -- ^ 'Optic' that can be lens
    -> (a -> b)  -- ^ Field modification function
    -> source  -- ^ Object where we want to set value
    -> target  -- ^ Resulting object with the modified field
over = id
{-# INLINE over #-}

{- | Gets a value out of a structure using a getter.

@since 0.0.0.0
-}
view
    :: (p ~ Fun (Const a))
    => Optic p source target a b  -- ^ 'Optic' that can be lens
    -> source  -- ^ Object from which we want to get value
    -> a  -- ^ Field of @source@
view opt = coerce (opt (Fun Const))
{-# INLINE view #-}
-- view opt = getConst . unFun (opt (Fun Const))
-- opt :: Fun (Const a) a b -> Fun (Const a) s t
-- opt :: (a -> Const a b) -> ( s -> Const a t)

{- | Creates 'Lens' from the getter and setter.

@since 0.0.0.0
-}
lens
    :: (source -> a)  -- ^ Getter
    -> (source -> b -> target)  -- ^ Setter
    -> Lens source target a b
lens getter setter = dimap (\s -> (s, getter s)) (uncurry setter) . second
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

{- | 'Lens'' for a tuple on the first argument.

>>> view fstL (42, "str")
42

@since 0.0.0.0
-}
fstL :: Lens (a, c) (b, c) a b
fstL = lens fst $ \(_, b) new -> (new, b)
{-# INLINE fstL #-}

{- | 'Lens'' for a tuple on the second argument.

>>> view sndL (42, "Hello")
"Hello"

@since 0.0.0.0
-}
sndL :: Lens (x, a) (x, b) a b
sndL = lens snd $ \(a, _) new -> (a, new)
{-# INLINE sndL #-}

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

{- | 'Prism' represents composable constructors and deconstructors.

'Prism' is an @'Optic' p@ with 'Choice' constraint on the @p@ type
variable.

@
                   +---> Current object
                   |
                   |      +-> Final object
                   |      |
                   +      +
        type Prism source target a b
                                 + +
                                 | |
 Field in current constructor <--+ |
                                   |
Field in final constructor <-------+
@

@since 0.0.0.0
-}
type Prism source target a b = forall p . Choice p => Optic p source target a b

{- | The monomorphic prisms which don't change the type of the container (or of
the value inside).

  * @a@ is the value inside the particular constructor
  * @source@ is some sum type

@since 0.0.0.0
-}
type Prism' source a = Prism source source a a

{- | Newtype around function @a -> r@. It's called /forget/ because it
forgets about its last type variable.

@since 0.0.0.0
-}
newtype Forget r a b = Forget
    { unForget :: a -> Maybe r
    }

-- | @since 0.0.0.0
instance Functor (Forget r x) where
    fmap :: (a -> b) -> Forget r x a -> Forget r x b
    fmap _ = coerce
    {-# INLINE fmap #-}

-- | @since 0.0.0.0
instance Profunctor (Forget r) where
    dimap :: (a -> b) -> (c -> d) -> Forget r b c -> Forget r a d
    dimap ab _cd (Forget br) = Forget (br . ab)
    {-# INLINE dimap #-}

-- | @since 0.0.0.0
instance Strong (Forget r) where
    first :: Forget r a b -> Forget r (a, c) (b, c)
    first (Forget ar) = Forget (ar . fst)
    {-# INLINE first #-}

    second :: Forget r a b -> Forget r (c, a) (c, b)
    second (Forget ar) = Forget (ar . snd)
    {-# INLINE second #-}

-- | @since 0.0.0.0
instance Choice (Forget r) where
    left :: Forget r a b -> Forget r (Either a c) (Either b c)
    left (Forget ar) = Forget (either ar (const Nothing))
    {-# INLINE left #-}

    right :: Forget r a b -> Forget r (Either c a) (Either c b)
    right (Forget ar) = Forget (either (const Nothing) ar)
    {-# INLINE right #-}

-- | @since 0.0.0.0
instance Monoidal (Forget r) where
    pappend :: Forget r a b -> Forget r c d -> Forget r (a, c) (b, d)
    pappend (Forget ar) (Forget cr) = Forget
        (\(a, c) -> getFirst $  First (ar a) <> First (cr c))
    {-# INLINE pappend #-}

    pempty :: Forget r a a
    pempty = Forget (const Nothing)
    {-# INLINE pempty #-}

{- | Match a value from @source@ type.

@since 0.0.0.0
-}
preview
    :: forall a source p
    .  (p ~ Forget a)
    => Optic p source source a a  -- ^ 'Optic' that can be prism
    -> source  -- ^ Object (possible sum type)
    -> Maybe a  -- ^ Value of type @a@ from a specific constructor
preview paapss = coerce (paapss wrap)
  where
    wrap :: Forget a a a
    wrap = coerce @(a -> Maybe a) @(Forget a a a) Just
    {-# INLINE wrap #-}
{-# INLINE preview #-}
-- preview paapss = getFirst . unForget (paapss (Forget (First . Just)))
-- paapss :: Forget (First a) a a -> Forget (First a) source source
-- paapss :: (a -> First a) -> source -> First a
-- paapss :: (a -> Maybe a) -> source -> Maybe a

{- | Create 'Prism' from constructor and matching function.

@since 0.0.0.0
-}
prism
    :: (b -> target)  -- ^ Constructor
    -> (source -> Either target a)  -- ^ Matching function
    -> Prism source target a b
-- prism :: (b -> target) -> (source -> Either target a) -> p a b -> p source target
prism ctor match = dimap match (either id ctor) . right
{-# INLINE prism #-}

{- | Create monomorphic 'Prism'' from constructor and matching function.

@since 0.0.0.0
-}
prism'
    :: (a -> source)  -- ^ Constructor
    -> (source -> Maybe a)  -- ^ Matching function
    -> Prism' source a
prism' ctor match = prism ctor (\s -> maybe (Left s) Right $ match s)
{-# INLINE prism' #-}

{- | 'Prism' for a 'Just' of 'Maybe'.

>>> preview _Just (Just 42)
Just 42

>>> preview _Just Nothing
Nothing

@since 0.0.0.0
-}
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ \case
    Just a  -> Right a
    Nothing -> Left Nothing
{-# INLINE _Just #-}


{- | 'Prism' for a 'Left' of 'Either'.

>>> preview _Left (Left 42)
Just 42

>>> preview _Left (Right "Hello")
Nothing

@since 0.0.0.0
-}
_Left :: Prism (Either a x) (Either b x) a b
_Left = prism Left $ \case
    Left l  -> Right l
    Right r -> Left $ Right r
{-# INLINE _Left #-}

{- | 'Prism' for a 'Left' of 'Either'.

>>> preview _Right (Left 42)
Nothing

>>> preview _Right (Right "Hello")
Just "Hello"

@since 0.0.0.0
-}
_Right :: Prism (Either x a) (Either x b) a b
_Right = prism Right $ \case
    Right a -> Right a
    Left x  -> Left $ Left x
{-# INLINE _Right #-}


{- | 'Traversal' provides composable ways to visit different parts of
a data structure.

'Traversal' is an @'Optic' p@ with the 'Choice' and 'Monoidal'
constraints on the @p@ type variable.

@
               +---> Current collection
               |
               |      +-> Final collection
               |      |
               +      +
type Traversal source target a b
                             + +
                             | |
          Current element <--+ |
                               |
         Final element <-------+
@

@since 0.0.0.0
-}
type Traversal source target a b
    = forall p
    . (Choice p, Monoidal p)
    => Optic p source target a b

{- | Traverse a data structure using given 'Traversal'.

>>> traverseOf eachPair putStrLn ("Hello", "World!")
Hello
World!
((),())

@since 0.0.0.0
-}
traverseOf
    :: (Applicative f, p ~ Fun f)
    => Optic p source target a b  -- ^ 'Optic' that can be a traversal
    -> (a -> f b)  -- ^ Traversing function
    -> source  -- ^ Data structure to traverse
    -> f target  -- ^ Traversing result
traverseOf pabPst aFb = unFun (pabPst (Fun aFb))
-- pabPst :: Fun f a b -> Fun f source target
-- pabPst :: (a -> f b) -> Fun f source target

{- | 'Traversal' for a pair of same type elements.

>>> over eachPair (+ 1) (3, 7)
(4,8)

@since 0.0.0.0
-}
eachPair :: Traversal (a, a) (b, b) a b
eachPair pab = pappend pab pab

{- | 'Traversal' for a 'Maybe'.

>>> over eachMaybe (+ 1) (Just 3)
Just 4
>>> over eachMaybe (+ 1) Nothing
Nothing

@since 0.0.0.0
-}
eachMaybe :: Traversal (Maybe a) (Maybe b) a b
eachMaybe pab = dimap maybeToEither eitherToMaybe (left pab)
  where
    maybeToEither :: Maybe a -> Either a ()
    maybeToEither = \case
        Just a  -> Left a
        Nothing -> Right ()

    eitherToMaybe :: Either a () -> Maybe a
    eitherToMaybe = \case
        Left a   -> Just a
        Right () -> Nothing

{- | 'Traversal' for lists.

>>> over eachList (+ 1) [1..5]
[2,3,4,5,6]
>>> over eachList (+ 1) []
[]

@since 0.0.0.0
-}
eachList :: Traversal [a] [b] a b
eachList pab = dimap listToEither eitherToList $ left $ pappend pab (eachList pab)
  where
    listToEither :: [a] -> Either (a, [a]) ()
    listToEither = \case
        []   -> Right ()
        x:xs -> Left (x, xs)

    eitherToList :: Either (a, [a]) () -> [a]
    eitherToList = \case
        Right ()     -> []
        Left (x, xs) -> x:xs
