module AdventOfCode.Twenty24.Util
  ( between'
  , dec
  , eol
  , genericParser
  , hSqrt
  , inc
  , last2
  , lines
  , lookupWithDefault
  , modify
  , multiline
  , oneOf'
  , oneOfChar
  , oneOfString
  , penultimate
  , range'
  , skip
  , spaced
  , sumMap
  , tally
  , testParser
  , to2dArray
  , unsafeFromRight
  , unsafeParse
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty (NonEmptyArray, singleton, snoc)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Lens (lastOf, traversed)
import Data.List (List, init, last, unsnoc)
import Data.List.Lazy as Lazy
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.String (split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Symbol (class IsSymbol)
import Data.Traversable (class Foldable, class Traversable, foldl)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Error)
import Parsing (Parser, runParser)
import Parsing.Combinators (choice, sepBy1, sepEndBy, try, (<?>), (<|>))
import Parsing.String (char, string)
import Partial.Unsafe (unsafePartial)
import PointFree ((<..))
import Prim.Row (class Cons)
import Record as Rec
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

sumMap :: forall a f. Functor f => Foldable f => (a -> Int) -> f a -> Int
sumMap = foldl add 0 <.. map

to2dArray :: String -> Array (Array Char)
to2dArray = map toCharArray <<< lines

lines :: String -> Array String
lines = split (Pattern "\n")

skip :: String -> Parser String Unit
skip = string >=> const (pure unit)

-- Test if a number is within `length` numbers of `start`. `start` counts as 1
-- `between' 10 0 x` is like `x in []`
-- `between' 10 1 x` is like `x in [10]`
-- `between' 10 2 x` is like `x in [10, 11]`
-- `between' 10 5 x` is like `x in [10, 11, 12, 13, 14]`
between' :: forall a. Ord a => Ring a => a -> a -> a -> Boolean
between' start length
  | length <= zero = const false
  | otherwise = between start (start + length - one)

range' :: forall a. Ord a => Ring a => a -> a -> NonEmptyArray a
range' start length = go (singleton start) (start + one) (length - one)
  where
  go a n l
    | l <= zero = a
    | otherwise = go (snoc a n) (n + one) (l - one)

-- heron's formula for sqrt of any euclidean ring
hSqrt :: forall a. Eq a => EuclideanRing a => a -> a
hSqrt n = go one zero
  where
  go i prev
    | i == prev = i
    | otherwise = go ((i + n / i) / (one + one)) i

inc :: forall a. Semiring a => a -> a
inc = add one

dec :: forall a. Ring a => a -> a
dec = (_ - one)

modify
  :: forall @l r1 r2 r a b
   . IsSymbol l
  => Cons l a r r1
  => Cons l b r r2
  => (a -> b)
  -> Record r1
  -> Record r2
modify = Rec.modify (Proxy :: _ l)

testParser
  :: forall m a
   . MonadThrow Error m
  => Show a
  => Eq a
  => String
  -> a
  -> Parser String a
  -> m Unit
testParser input expected parser =
  runParser input parser `shouldEqual` Right expected

oneOfChar
  :: forall a t
   . Traversable t
  => Show (t Char)
  => t (Tuple Char a)
  -> Parser String a
oneOfChar = oneOf' char

oneOfString
  :: forall a t
   . Traversable t
  => Show (t String)
  => t (Tuple String a)
  -> Parser String a
oneOfString = oneOf' string

oneOf'
  :: forall a1 a2 s t
   . Traversable t
  => Show (t a1)
  => (a1 -> Parser s a1)
  -> t (Tuple a1 a2)
  -> Parser s a2
oneOf' p xs = choice parsers <?> expected
  where
  parsers = map parser xs
  parser (Tuple c x) = try (p c $> x)
  cs = map fst xs
  expected = "one of " <> show cs

genericParser :: forall @a. BoundedEnum a => Show a => Parser String a
genericParser = oneOfString $ map (\a -> Tuple (show a) a) as
  where
  as :: Array a
  as = enumFromTo bottom top

penultimate :: forall a. List a -> Maybe a
penultimate = join <<< map (lastOf traversed) <<< init

last2 :: forall a. List a -> Maybe (Tuple a a)
last2 l = do
  uns <- unsnoc l
  penult <- last uns.init
  pure $ Tuple penult uns.last

unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight = unsafePartial fromRight
  where
  fromRight :: Partial => Either a b -> b
  fromRight (Right b) = b

unsafeParse :: forall a. String -> Parser String a -> a
unsafeParse inp par = unsafeFromRight $ runParser inp par

-- create a map of how many times each element occurs in a list
tally :: forall f k. Ord k => Foldable f => f k -> Map k Int
tally = Map.fromFoldableWith (+) <<< flip Lazy.zip ones <<< Lazy.fromFoldable
  where
  ones = Lazy.repeat 1

lookupWithDefault :: forall k v. Ord k => v -> k -> Map k v -> v
lookupWithDefault v = fromMaybe v <.. Map.lookup

spaced :: forall a. Parser String a -> Parser String (NonEmptyList a)
spaced = (_ `sepBy1` string " ")

multiline :: forall a. Parser String a -> Parser String (List a)
multiline = (_ `sepEndBy` eol)

eol :: Parser String String
eol = string "\n" <|> string "\r\n"