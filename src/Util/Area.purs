module AdventOfCode.Util.Area
  ( Area
  , filled
  , fromFoldable
  , get
  , set
  ) where

import AdventOfCode.Prelude hiding (wrap, unwrap)

import AdventOfCode.Util.Coord (Coord(..))
import Data.Array (fromFoldable) as Array
import Data.Array (replicate)
import Data.Lens (preview, iso)
import Data.Lens (set) as Lens
import Data.Lens.Index (ix)
import Data.Lens.Types (AffineTraversal', Iso')

type Matrix a = Array (Array a)

newtype Area a = Area (Matrix a)

instance Show a => Show (Area a) where
  show (Area a) = intercalate "\n" $ foldMap show <$> a

unwrap :: forall a. Area a -> Matrix a
unwrap (Area a) = a

wrap :: forall a. Matrix a -> Area a
wrap = Area

filled :: forall a. { width :: Int, height :: Int, default :: a } -> Area a
filled { width, height, default } =
  Area $ replicate height $ replicate width default

fromFoldable
  :: forall a h w. Functor h => Foldable h => Foldable w => h (w a) -> Area a
fromFoldable = Area <<< Array.fromFoldable <<< map Array.fromFoldable

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
