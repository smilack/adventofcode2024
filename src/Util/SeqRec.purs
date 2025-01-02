module AdventOfCode.Twenty24.Util.SeqRec
  ( (<>?)
  , addseq
  , class RecordOfAp
  , class RecordToList
  , main
  , seqrec
  ) where

import AdventOfCode.Prelude hiding (sequence)

import Data.Symbol (class IsSymbol)
import Debug (traceM)
import Prim.RowList (RowList)
import Record (get, insert, set)
import Record.Traversable (sequence, class RecordToRow)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (class ListToRow, class RowToList, Cons, Nil)

main :: Effect Unit
main = do
  log "SeqRec testing"
  traceM $ sequence @FooR { foo: Just 2, bar: Just 8 }
  traceM $ sequence @FooR { foo: Nothing, bar: Just 8 }
  traceM $ sequence @FooR { foo: Just 2, bar: Nothing }
  traceM $ sequence @FooR { foo: Nothing, bar: Nothing }
  traceM x
  log "End"

x :: Maybe FooR
x = sequence { foo: Just 2, bar: Just 8 }

-- testRecAp
--   :: ?e -- RecAp FooMaybe (Cons "foo" (Maybe Int) (Cons "bar" (Maybe Int) Nil))
-- testRecAp = RecAp { foo: Just 2, bar: Just 8 }

-- where
-- -- foo :: Record FooMaybe
-- foo = { foo: Just 2, bar: Just 8 }

type FooMaybe = (foo :: Maybe Int, bar :: Maybe Int)
type Foo = (foo :: Int, bar :: Int)

type FooR = { foo :: Int, bar :: Int }

-- ┌────────────────────────────────────────────────────────────┐
-- │ RecAp newtype                                              │
-- ├────────────────────────────────────────────────────────────┤
-- │ Potentially useful for base case instances with empty rows │
-- └────────────────────────────────────────────────────────────┘

data RecAp :: Row Type -> RowList Type -> Type
data RecAp row list = RecAp
  (RowToList row list => ListToRow list row => Record row)

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Helper classes/instances (RecordToList, RecordOfAp, UnApRecord)           │
-- ├───────────────────────────────────────────────────────────────────────────┤
-- │ RecordToList                                                              │
-- │   Equate a Record, Row, and RowList                                       │
-- │                                                                           │
-- │ RecordOfAp                                                                │
-- │   Record where every value type is in an Applicative @applic.             │
-- │                                                                           │
-- │ UnApRecord                                                                │
-- │   For RowLists @listAp and @listNoApp, if @applic @listAp has an instance │
-- │   of RecordOfAp, then @listNoAp is equivalent to @listAp with all value   │
-- │   types unwrapped from @applic.                                           │
-- └───────────────────────────────────────────────────────────────────────────┘

class RecordToList :: Type -> Row Type -> RowList Type -> Constraint
class
  ( RecordToRow rec row
  , RowToList row list
  ) <=
  RecordToList rec row list
  | rec -> row list
  , row -> rec list
  , list -> rec row

instance (RowToList row list) => RecordToList { | row } row list

class RecordOfAp
  :: (Type -> Type) -> Type -> Row Type -> RowList Type -> Constraint
class
  RecordToList rec row list <=
  RecordOfAp applic rec row list
  | rec -> row list
  , row -> rec list
  , list -> rec row

instance
  ( Applicative f
  , IsSymbol key
  , RecordToList rec row list
  , RowToList row (Cons key (f a) Nil)
  ) =>
  RecordOfAp f rec row list

else instance
  ( Applicative f
  , IsSymbol key
  , RecordToList rec row list
  , RowToList row (Cons key (f a) list')
  , RecordOfAp f { | row' } row' list'
  ) =>
  RecordOfAp f rec row list

class UnApRecord :: (Type -> Type) -> RowList Type -> RowList Type -> Constraint
class UnApRecord applic listAp listNoAp

instance Applicative f => UnApRecord f Nil Nil
instance
  ( Applicative f
  , UnApRecord f restAp restNoAp
  ) =>
  UnApRecord f (Cons key (f a) restAp) (Cons key a restNoAp)

-- ┌─────────────────────────────────────────────────────────────────┐
-- │ seqrec, addseq, <>?                                             │
-- ├─────────────────────────────────────────────────────────────────┤
-- │ seqrec { a: Just 1 } <>? { b: Just "yes" } <>? { c: Just true } │
-- ├─────────────────────────────────────────────────────────────────┤
-- │   == Just { a: 1, b: "yes", c: true }                           │
-- └─────────────────────────────────────────────────────────────────┘

seqrec
  :: forall k v r0 r1
   . IsSymbol k
  => Cons k (Maybe v) () r0
  => RowToList r0 (Cons k (Maybe v) Nil)
  => Cons k v () r1
  => Record r0
  -> Maybe (Record r1)
seqrec r0 = (\val -> set key val r0) <$> get key r0
  where
  key = Proxy @k

addseq
  :: forall k v r0 r1 r2
   . IsSymbol k
  => Cons k (Maybe v) () r0
  => RowToList r0 (Cons k (Maybe v) Nil)
  => Lacks k r1
  => Cons k v r1 r2
  => Maybe (Record r1)
  -> Record r0
  -> Maybe (Record r2)
addseq mr1 r0 = (\r1 val -> insert key val r1) <$> mr1 <*> get key r0
  where
  key = Proxy @k

infixl 5 addseq as <>?
