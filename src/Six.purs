module AdventOfCode.Twenty24.Six
  ( LabCell(..)
  , main
  , toMap
  ) where

import AdventOfCode.Prelude

import AdventOfCode.Twenty24.Util (multiline, oneOfChar)
import AdventOfCode.Twenty24.Util.SeqRec (seqrec, (<>?))
import AdventOfCode.Util.Area (Area)
import AdventOfCode.Util.Area as Area
import AdventOfCode.Util.Coord (Coord(..), Direction(..))
import Data.Array as Array
import Data.FoldableWithIndex (findWithIndex)
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
import Record (get, insert, merge, modify, set, union, delete)
import Type.Proxy (Proxy(..))
import Type.Row (type (+), class Lacks, class Cons, class Union)
import Type.RowList as RL

-- import Type.RowList (Cons, Nil, class ListToRow)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/6"
  liftEffect do
    log "Part 1:"
    -- logShow $ solve1 input
    log "Part 2:"
    -- logShow $ solve2 input
    log "End"
    --
    -- log "\nhi"
    -- -- log $ show @(ShowKeysOnly State StateList) $ ShowKeysOnly
    -- --   { pos: false, lab: 0, dir: "a" }
    --
    log "\nhi"
    logShow $ seqrec { pos: Just false }
      <>? { lab: Just 0 }
      <>? { dir: Just "a" }

--
newtype ShowKeysOnly :: Row Type -> RL.RowList Type -> Type
newtype ShowKeysOnly r l = ShowKeysOnly (Record r)

class ShowKeysInRowList2 :: Row Type -> RL.RowList Type -> Constraint
class RL.RowToList r l <= ShowKeysInRowList2 r l | l -> r where
  buildKeyList2 :: Record r -> List String

instance ShowKeysInRowList2 () RL.Nil where
  buildKeyList2 _ = Nil

instance
  ( Reflectable sym String
  , ShowKeysInRowList2 r' rest
  , IsSymbol sym
  , Cons sym k r' r
  , RL.RowToList r (RL.Cons sym k rest)
  , Lacks sym r'
  ) =>
  ShowKeysInRowList2 r (RL.Cons sym k rest)
  where
  buildKeyList2 :: Record r -> List String
  buildKeyList2 rec = (Cons :: String -> List String -> List String) thisKey
    remainingKeys
    where
    thisKey = reflectType (Proxy @sym)

    _ = spy "get thisKey rec" $ get (Proxy @sym) rec

    restRec = delete (Proxy @sym) rec

    remainingKeys = buildKeyList2 @r' @rest restRec

instance
  ( ShowKeysInRowList2 recordRows rowList
  ) =>
  Show (ShowKeysOnly recordRows rowList)
  where
  show (ShowKeysOnly rec) = show $ buildKeyList2 @recordRows @rowList rec

-- type State =
--   { pos :: Coord
--   , lab :: Area LabCell
--   , dir :: Direction
--   }
type State =
  ( dir :: String
  , lab :: Int
  , pos :: Boolean
  )

type StateList =
  (RL.Cons "dir" String (RL.Cons "lab" Int (RL.Cons "pos" Boolean RL.Nil)))

addrec
  :: forall l a r1 r2
   . IsSymbol l
  => Lacks l r1
  => Cons l a r1 r2
  => Tuple (Proxy l) a
  -> Record r1
  -> Record r2
addrec (l /\ a) = insert l a

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
parseLab s = fold $ runParser s lab

lab :: Parser String (List (List LabCell))
lab = multiline row

row :: Parser String (List LabCell)
row = many $ oneOfChar labCellCharacters
