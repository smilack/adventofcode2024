module AdventOfCode.Prelude (module Exports) where

-- Ctrl + K, Ctrl + Shift + S: save without formatting

import Prelude
  ( class Applicative, class Apply, class Bind, class BooleanAlgebra
  , class Bounded, class Category, class CommutativeRing, class Discard
  , class DivisionRing, class Eq, class EuclideanRing, class Field
  , class Functor, class HeytingAlgebra, class Monad, class Monoid, class Ord
  , class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show
  , type (~>), Ordering(..), Unit, Void, absurd, add, ap, append, apply, between
  , bind, bottom, clamp, compare, comparing, compose, conj, const, degree
  , discard, disj, div, eq, flap, flip, gcd, identity, ifM, join, lcm, liftA1
  , liftM1, map, max, mempty, min, mod, mul, negate, not, notEq, one, otherwise
  , pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero
  , (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$)
  , (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=)
  , (>=>), (>>=), (>>>), (||)
  ) as Exports