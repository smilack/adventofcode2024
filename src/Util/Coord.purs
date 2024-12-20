module AdventOfCode.Util.Coord
  ( Coord(..)
  , Direction(..)
  , dist
  , mkCoordRC
  , mkCoordXY
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
  show (Coord { x, y }) = "(row " <> show y <> ", column " <> show x <> ")"

mkCoordXY :: Int -> Int -> Coord
mkCoordXY = Coord <.. { x: _, y: _ }

mkCoordRC :: Int -> Int -> Coord
mkCoordRC = Coord <.. { y: _, x: _ }

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

dist :: Coord -> Coord -> { x :: Int, y :: Int }
dist (Coord a) (Coord b) =
  { x: a.x - b.x
  , y: a.y - b.y
  }
