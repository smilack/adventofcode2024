module AdventOfCode.Util.Coord
  ( Coord(..)
  , Direction(..)
  , move
  ) where

import AdventOfCode.Prelude hiding (modify)
import AdventOfCode.Twenty24.Util (modify)

newtype Coord = Coord { x :: Int, y :: Int }

derive instance Newtype Coord _
derive instance Eq Coord

instance Ord Coord where
  compare (Coord a) (Coord b) =
    case compare a.y b.y of
      EQ -> compare a.x b.x
      order -> order

instance Show Coord where
  show (Coord { x, y }) = "(" <> show x <> "," <> show y <> ")"

data Direction = Up | Right | Down | Left

instance Show Direction where
  show = case _ of
    Up -> "↑"
    Right -> "→"
    Down -> "↓"
    Left -> "←"

move :: Direction -> Coord -> Coord
move = over Coord <<< case _ of
  Up -> modify @"y" dec
  Right -> modify @"x" inc
  Down -> modify @"y" inc
  Left -> modify @"x" dec
  where
  inc = (_ + 1)
  dec = (_ - 1)
