# prolens

![Prolens Logo](https://user-images.githubusercontent.com/8126674/95865685-da91b080-0d5e-11eb-91cd-b6a7bae29262.png)

[![GitHub CI](https://github.com/kowainik/prolens/workflows/CI/badge.svg)](https://github.com/kowainik/prolens/actions)
[![Hackage](https://img.shields.io/hackage/v/prolens.svg?logo=haskell)](https://hackage.haskell.org/package/prolens)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

The `prolens` package is a Haskell library with a __minimal__ and
__lightweight__ implementation of _optics_. __Optic__ is a high-level
concept for values providing _composable_ access to different parts of
structures.

Prolens provides the following optics:

* __Lens__ — composable getters and setters
* __Prisms__ — composable constructors and deconstructors

## Goals

We created the `prolens` project in pursuit of the following goals:

1. __Education__. Teach others how to implement and work with
   profunctor optics. This also means that some underlying types or
   type variables have different unconventional names
2. __Learning__. Explore new concepts ourselves and understand better
   abstractions used in the implementation.
3. __Minimalism__. Keep the number of dependencies, features and code
   low, but still solve common problems.
4. __Performance__. Despite being minimalist, implement optics so they
   are as fast as manual and clumsy pattern matching.
5. __Exploration__. Understand how different modern Haskell features
   can work on improving interface and bring new flavour into standard
   approaches. Because of this, we implement our own `Profunctor`
   typeclass with the
   [QuantifiedConstraints](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#quantified-constraints)
   feature, which is not present in any other library at the moment.
6. __Profunctors__. We use profunctor encoding of optics because it
   has more elegant design with fewer surprises.

## Features

1. __Lightweight__. Only
   [base](http://hackage.haskell.org/package/base)
   in dependencies. The project itself also has a rather small amount
   of code.
2. __Fast__. Despite being lightweight, `prolens` provides a
   performant API. We use the
   [inspection-testing](https://hackage.haskell.org/package/inspection-testing)
   library to guarantee that our implementation of optics compiles to
   the same code as plain Haskell getters, record-update syntax and
   pattern matching.
3. __Excellent documentation__. The `prolens` library contains a
   mini-tutorial on optics, enough to understand how and when to use
   basic lenses and prisms.
4. __Beginner-friendly__. The abstractions in the implementation are
   hardcore, but our documentation presents the concept in a
   beginner-friendly and approachable manner.
5. __Lawful__. We use property-based testing to make sure that laws of
   all underlying abstractions are verified.

## How to use

`prolens` is compatible with the latest GHC compiler
versions starting from `8.6.5`.

In order to start using `prolens` in your project, you
will need to set it up with the three easy steps:

1. Add the dependency on `prolens` in your project's
   `.cabal` file. For this, you should modify the `build-depends`
   section by adding the name of this library. After the adjustment,
   this section could look like this:

   ```haskell
   build-depends: base ^>= 4.14
                , prolens ^>= 0.0
   ```
2. In the module where you wish to use composable getters and setters,
   you should add the import:

   ```haskell
   import Prolens (Lens', lens, view)
   ```
3. Now you can use the types and functions from the library:

   ```haskell
   data User = User
       { userName :: String
       , userAge  :: Int
       }

   nameL :: Lens' User String
   nameL = lens userName (\u new { u = userName = new })

   main :: IO ()
   main = putStrln $ view nameL (User "Johnny" 27)
   ```

### Usage with Stack

If `prolens` is not available on your current Stackage
resolver yet, fear not! You can still use it from Hackage by adding
the following to the `extra-deps` section of your `stack.yaml` file:

```yaml
extra-deps:
  - prolens-0.0.0.0
```

## Comparison to other libraries

1. [lens](https://hackage.haskell.org/package/lens)

   It is the most mature Haskell library for optics. `lens` provides a
   richer interface, but it is heavyweight and based on Van Laarhoven (VL)
   encoding of lenses.

2. [microlens](https://hackage.haskell.org/package/microlens)

   A lightweight implementation of optics compatible with
   `lens`. `microlens` is also minimalistic, but it doesn't provide
   prisms and is based on VL encoding.

3. [optics](https://hackage.haskell.org/package/optics)

   The `optics` library uses the profunctor encoding. It provides much
   more features than `prolens`, but at the same time it's
   heavyweight. Also, `optics` uses an opaque representation of optics
   (e.g. they are wrapped in a newtype), which means that they are
   composed using the custom operator `%`, while in `prolens` optics
   are type aliases to functions and can be easily composed with the
   dot `.` operator.

4. [profunctor-optics](https://hackage.haskell.org/package/profunctor-optics)

   This library is also based on profunctor encoding (as the name
   suggests) and provides optics as aliases to functions. But it is
   more heavyweight, though it provides more features.

In addition to this per-library comparison, `prolens` has a few unique
features:

  * Beginner-friendly documentation with usage examples
  * Usage of `inspection-testing` to guarantee the performance of
    optics
  * Property-based tests of lens and typeclasses laws to make sure
    that all abstractions behave properly

## Acknowledgement

  * Edward Kmett for lenses and profunctor typeclasses
  * Well-Typed for the implementation of `optics`
