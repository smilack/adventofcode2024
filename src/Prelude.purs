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

import Data.Either (Either, fromRight) as Exports

import Data.Generic.Rep (class Generic) as Exports
import Data.Show.Generic (genericShow) as Exports

import Data.List (List(..), (:)) as Exports
import Data.List.Types (NonEmptyList) as Exports

import Data.Maybe (Maybe(..)) as Exports

import Data.Map (Map) as Exports

import Data.Newtype
  (wrap, unwrap, class Newtype, un, modify, ala, over, under) as Exports

import Data.Tuple (Tuple(..), uncurry) as Exports
import Data.Tuple.Nested (type (/\), (/\)) as Exports

import Data.Traversable
  (class Foldable, class Traversable, traverse, sequence, for, foldl, foldr
  , foldMap, fold
  ) as Exports

import Effect (Effect) as Exports
import Effect.Aff (launchAff_) as Exports
import Effect.Class (liftEffect) as Exports
import Effect.Console (log, logShow) as Exports

import Parsing (Parser, runParser) as Exports
import Parsing.Combinators (many, try) as Exports

import PointFree ((<..)) as Exports
