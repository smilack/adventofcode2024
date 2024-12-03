module AdventOfCode.Twenty24.One.Pair (Pair(..)) where

import Prelude
import Control.Apply (lift2)
import Data.Traversable (class Foldable, class Traversable, traverseDefault)
import PointFree ((<..))

data Pair a = Pair { a :: a, b :: a }

instance Functor Pair where
  map f (Pair { a, b }) = Pair { a: f a, b: f b }

instance Apply Pair where
  apply (Pair f) (Pair { a, b }) = Pair { a: f.a a, b: f.b b }

instance Applicative Pair where
  pure x = Pair { a: x, b: x }

instance Foldable Pair where
  foldr :: forall a b. (a -> b -> b) -> b -> Pair a -> b
  foldr f init (Pair { a, b }) = f a (f b init)

  foldl :: forall a b. (b -> a -> b) -> b -> Pair a -> b
  foldl f init (Pair { a, b }) = f (f init a) b

  foldMap :: forall a m. Monoid m => (a -> m) -> Pair a -> m
  foldMap f (Pair { a, b }) = f a <> f b

instance Traversable Pair where
  traverse :: forall a b m. Applicative m => (a -> m b) -> Pair a -> m (Pair b)
  traverse = traverseDefault

  sequence :: forall a m. Applicative m => Pair (m a) -> m (Pair a)
  sequence (Pair { a, b }) = lift2 (Pair <.. { a: _, b: _ }) a b

instance Show a => Show (Pair a) where
  show (Pair { a, b }) = "{ a: " <> show a <> ", b: " <> show b <> " }"

instance Eq a => Eq (Pair a) where
  eq (Pair a) (Pair b) = a.a == b.a && a.b == b.b