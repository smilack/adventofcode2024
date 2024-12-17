module AdventOfCode.Util.Area
  ( Area
  , filled
  , fromFoldable
  , get
  , set
  ) where

import AdventOfCode.Prelude hiding (wrap, unwrap)

import AdventOfCode.Twenty24.Util (dec, inc)
import AdventOfCode.Util.Coord (Coord(..), mkCoordRC)
import Data.Array (fromFoldable) as Array
import Data.Array (length, replicate, (!!))
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndex)
import Data.Lens (preview, iso)
import Data.Lens (set) as Lens
import Data.Lens.Index (ix)
import Data.Lens.Types (AffineTraversal', Iso')
import Data.Traversable (sequenceDefault)

-- ┌──────────────────┐
-- │ Area type        │
-- ├──────────────────┤
-- │ Show instance    │
-- ├──────────────────┤
-- │ Functor instance │
-- └──────────────────┘

type Matrix a = Array (Array a)

newtype Area a = Area (Matrix a)

instance Show a => Show (Area a) where
  show (Area a) = intercalate "\n" $ foldMap show <$> a

instance Functor Area where
  map f (Area a) = Area $ map (map f) a

-- ┌───────────────────┐
-- │ Foldable instance │
-- └───────────────────┘

instance Foldable Area where
  foldl = foldlArea
  foldr = foldrArea
  foldMap f (Area a) = foldMap (foldMap f) a

foldrArea :: forall a b. (a -> b -> b) -> b -> Area a -> b
foldrArea abb b a@(Area m) = foldMatrix (length m - 1) dec (foldr abb) b a

foldlArea :: forall a b. (b -> a -> b) -> b -> Area a -> b
foldlArea bab = foldMatrix 0 inc (foldl bab)

foldMatrix
  :: forall a b. Int -> (Int -> Int) -> (b -> Array a -> b) -> b -> Area a -> b
foldMatrix start next f b (Area m) = go start b
  where
  go i b' = case m !! i of
    Nothing -> b'
    Just a -> go (next i) (f b' a)

-- ┌────────────────────────────┐
-- │ FoldableWithIndex instance │
-- └────────────────────────────┘

instance FoldableWithIndex Coord Area where
  foldrWithIndex = foldrWithIndexArea
  foldlWithIndex = foldlWithIndexArea
  foldMapWithIndex = foldMapWithIndexDefaultL

foldrWithIndexArea :: forall a b. (Coord -> a -> b -> b) -> b -> Area a -> b
foldrWithIndexArea iab2b b a@(Area m) =
  foldMatrixWithIndex (length m - 1) dec f b a
  where
  f r = foldrWithIndex (iab2b <<< mkCoordRC r)

foldlWithIndexArea :: forall a b. (Coord -> b -> a -> b) -> b -> Area a -> b
foldlWithIndexArea iba2b = foldMatrixWithIndex 0 inc f
  where
  f r = foldlWithIndex (iba2b <<< mkCoordRC r)

foldMatrixWithIndex
  :: forall a b
   . Int
  -> (Int -> Int)
  -> (Int -> b -> Array a -> b)
  -> b
  -> Area a
  -> b
foldMatrixWithIndex start next f b (Area m) = go start b
  where
  go y b' = case m !! y of
    Nothing -> b'
    Just a -> go (next y) (f y b' a)

-- ┌──────────────────────┐
-- │ Traversable instance │
-- └──────────────────────┘

instance Traversable Area where
  traverse = traverseMatrix
  sequence = sequenceDefault

traverseMatrix
  :: forall a b m. Applicative m => (a -> m b) -> Area a -> m (Area b)
traverseMatrix a2mb (Area a) = Area <$> traverse (traverse a2mb) a

-- ┌──────────────────────────┐
-- │ Pseudo-newtype functions │
-- └──────────────────────────┘

unwrap :: forall a. Area a -> Matrix a
unwrap (Area a) = a

wrap :: forall a. Matrix a -> Area a
wrap = Area

-- ┌──────────────┐
-- │ Constructors │
-- └──────────────┘

filled :: forall a. { width :: Int, height :: Int, default :: a } -> Area a
filled { width, height, default } =
  Area $ replicate height $ replicate width default

fromFoldable
  :: forall a h w. Functor h => Foldable h => Foldable w => h (w a) -> Area a
fromFoldable = Area <<< Array.fromFoldable <<< map Array.fromFoldable

-- ┌────────┐
-- │ Optics │
-- └────────┘

_area :: forall a. Iso' (Area a) (Matrix a)
_area = iso unwrap wrap

_cell :: forall a. Coord -> AffineTraversal' (Area a) a
_cell (Coord { x, y }) = _area <<< (ix y) <<< (ix x)

get :: forall a. Coord -> Area a -> Maybe a
get c = preview $ _cell c

set :: forall a. Coord -> a -> Area a -> Area a
set c = Lens.set $ _cell c

-- ⤺⤻
-- infix 1 set as ⤺
