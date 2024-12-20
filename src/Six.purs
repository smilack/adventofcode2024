module AdventOfCode.Twenty24.Six
  ( LabCell(..)
  , initialState
  , isRect
  , main
  , possibleCoords
  , run
  , solve1
  , solve2
  , toMap
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Util (multiline, oneOfChar)
import AdventOfCode.Twenty24.Util.SeqRec (seqrec, (<>?))
import AdventOfCode.Util.Area (Area)
import AdventOfCode.Util.Area (get, set) as A
import AdventOfCode.Util.Area as Area
import AdventOfCode.Util.Coord (Coord(..), Direction(..), move, dist)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..)) as E
import Data.FoldableWithIndex (findWithIndex)
import Data.List (length, take)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.Reflectable (class Reflectable, class Reifiable, reflectType, reifyType)
import Data.Symbol (class IsSymbol)
import Data.Typelevel.Num (d0, d1, d2, d3)
import Data.Vec (Vec, empty, (+>), (!!), sort)
import Debug (spy, spyWith, traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)
import Record (insert)
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

-- part 2
-- 1. starting from right side of an obstacle (r, c)
-- 2. go north until hitting an obstacle (r', c)
-- 3. go east until hitting an obstacle (r', c')
-- 4. go south until first of:
--    A. hitting an obstacle -> stop
--    B. (r, c') -> continue
-- 5. go west until first of
--    A. obscacle -> stop
--    B. (r, c) -> (r, c') is valid

solve2 :: String -> Int
solve2 = length <<< possibleCoords

possibleCoords :: String -> List Coord
possibleCoords = fold
  <<< map run2
  <<< initialState2
  <<< toMap

isRect :: Coord -> Coord -> Coord -> Coord -> Boolean
isRect a b c d =
  near top _.y (-1)
    && near right _.x 1
    && near left _.x 1
    && near bottom _.y (-1)
  where
  points = sort $ a +> b +> c +> d +> empty
  --
  top = spyWith "top" show $ (points !! d0) /\ (points !! d1)
  right = spyWith "right" show $ (points !! d1) /\ (points !! d3)
  bottom = spyWith "bottom" show $ (points !! d2) /\ (points !! d3)
  left = spyWith "left" show $ (points !! d0) /\ (points !! d2)
  --
  near pts f n = (f $ uncurry dist pts) == n

type State2 = Record (St (bumps :: List Coord, poss :: List Coord))

initialState2 :: Area LabCell -> Maybe State2
initialState2 =
  map
    (insert (Proxy @"poss") Nil <<< insert (Proxy @"bumps") Nil)
    <<< initialState

run2 :: State2 -> List Coord
run2 = go <<< E.Right
  where
  go = case _ of
    E.Left coords -> coords
    E.Right state@{ lab, pos, dir, bumps, poss } -> go $
      let
        peek = move dir pos
      in
        case A.get peek (spyWith "map" (("\n" <> _) <<< show) lab) of
          Just Obstructed -> E.Right $ state
            { dir = turn dir
            , bumps = peek : bumps
            , lab = A.set pos (Guard $ turn dir) lab
            }
          Just Empty -> E.Right $ nextMove $ checkPoss peek state
          Just Visited -> E.Right $ nextMove $ checkPoss peek state
          _ -> E.Left poss

  checkPoss :: Coord -> State2 -> State2
  checkPoss peek state = case state.bumps of
    (a : b : c : _) ->
      if isRect a b c peek then state
        { poss = spyWith "adding" show peek : state.poss }
      else state
    _ -> state

solve1 :: String -> Int
solve1 = unwrap
  <<< fold
  <<< map (foldMap $ Additive <<< isVisited)
  <<< map run
  <<< initialState
  <<< toMap

run :: State -> Area LabCell
run = go <<< E.Right
  where
  go = case _ of
    E.Left area -> area
    E.Right state@{ lab, pos, dir } -> go $
      case A.get (move dir pos) lab of
        Just Obstructed -> E.Right $ state { dir = turn dir }
        Just Empty -> E.Right $ nextMove state
        Just Visited -> E.Right $ nextMove state
        _ -> E.Left $ A.set pos Visited lab

nextMove :: forall r. Record (St r) -> Record (St r)
nextMove state@{ lab, pos, dir } = state
  { lab = A.set pos Visited $ A.set (move dir pos) (Guard dir) lab
  , pos = move dir pos
  }

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

type St r =
  ( pos :: Coord
  , lab :: Area LabCell
  , dir :: Direction
  | r
  )

type State = Record (St ())

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
