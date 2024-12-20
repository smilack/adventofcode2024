module AdventOfCode.Twenty24.Six
  ( LabCell(..)
  , initialState
  , main
  , run
  , solve1
  , toMap
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Util (multiline, oneOfChar)
import AdventOfCode.Twenty24.Util.SeqRec (seqrec, (<>?))
import AdventOfCode.Util.Area (Area)
import AdventOfCode.Util.Area (get, set) as A
import AdventOfCode.Util.Area as Area
import AdventOfCode.Util.Coord (Coord(..), Direction(..), move)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..)) as Either
import Data.FoldableWithIndex (findWithIndex)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.Reflectable (class Reflectable, class Reifiable, reflectType, reifyType)
import Data.Symbol (class IsSymbol)
import Debug (spy, traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)
import Type.Proxy (Proxy(..))
import Type.Row (type (+), class Lacks, class Cons, class Union)
import Type.RowList as RL

-- import Type.RowList (Cons, Nil, class ListToRow)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/6"
  liftEffect do
    log "Part 1:"
    logShow $ solve1 input
    log "Part 2:"
    -- logShow $ solve2 input
    log "End"

solve1 :: String -> Int
solve1 = unwrap
  <<< fold
  <<< map (foldMap $ Additive <<< isVisited)
  <<< map run
  <<< initialState
  <<< toMap

run :: State -> Area LabCell
run = step >>> case _ of
  Either.Left area -> area
  Either.Right state' -> run state'

step :: State -> Either (Area LabCell) State
step state@{ lab, pos, dir } = case A.get peekCoord lab of
  Just Obstructed -> Either.Right turned
  Just Empty -> Either.Right normalMove
  Just Visited -> Either.Right normalMove
  _ -> Either.Left end
  where
  peekCoord :: Coord
  peekCoord = move dir pos

  normalMove :: State
  normalMove = state
    { lab = A.set pos Visited $ A.set peekCoord (Guard dir) $ lab
    , pos = peekCoord
    }

  turned :: State
  turned = state { dir = turn dir }

  end :: Area LabCell
  end = A.set pos Visited lab

initialState :: Area LabCell -> Maybe State
initialState lab = seqrec { lab: Just lab } <>? { pos } <>? { dir }
  where
  start = findWithIndex isGuard =<< Just lab
  pos = _.index <$> start
  dir = directionOf =<< _.value <$> start

isVisited :: LabCell -> Int
isVisited = case _ of
  Visited -> 1
  _ -> 0

isGuard :: Coord -> LabCell -> Boolean
isGuard _ = case _ of
  Guard _ -> true
  _ -> false

directionOf :: LabCell -> Maybe Direction
directionOf = case _ of
  Guard d -> Just d
  _ -> Nothing

type State =
  { pos :: Coord
  , lab :: Area LabCell
  , dir :: Direction
  }

turn :: Direction -> Direction
turn = case _ of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up

data LabCell
  = Empty
  | Obstructed
  | Visited
  | Guard Direction

instance Show LabCell where
  show = case _ of
    Empty -> "░"
    Obstructed -> "█"
    Visited -> " "
    Guard d -> show d

labCellCharacters :: Array (Char /\ LabCell)
labCellCharacters =
  [ ('.' /\ Empty)
  , ('#' /\ Obstructed)
  , ('^' /\ Guard Up)
  , ('>' /\ Guard Down)
  , ('v' /\ Guard Right)
  , ('<' /\ Guard Left)
  ]

toMap :: String -> Area LabCell
toMap = Area.fromFoldable <<< parseLab

parseLab :: String -> List (List LabCell)
parseLab s = fold $ runParser s labP

labP :: Parser String (List (List LabCell))
labP = multiline row

row :: Parser String (List LabCell)
row = many $ oneOfChar labCellCharacters
