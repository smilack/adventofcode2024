module AdventOfCode.Twenty24.Util.SeqRec
  ( (<>?)
  , addseq
  , class RecordOfAp
  , main
  , seqrec
  ) where

import AdventOfCode.Prelude hiding (sequence)

import Data.Symbol (class IsSymbol)
import Debug (spy, traceM)
import Prim.RowList (RowList)
import Record (delete, get, insert, set)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (class RowListSet, class RowToList, class ListToRow, Cons, Nil)

main :: Effect Unit
main = do
  log "SeqRec testing"
  traceM $ test { foo: Just 2, bar: Just 8 }
  traceM $ test { foo: Just 2, bar: Nothing }
  traceM $ test { foo: Nothing, bar: Just 8 }
  log "End"

test :: Record FooMaybe -> Maybe (Record Foo)
test = sequenceRecord @TravRec @Maybe @FooMaybe @Foo <<< TravRec

type FooMaybe = (foo :: Maybe Int, bar :: Maybe Int)
type Foo = (foo :: Int, bar :: Int)

-- ┌─────────────────────────────────────────┐
-- │ TravRec newtype                         │
-- ├─────────────────────────────────────────┤
-- │ Has instance of TraversableRecord class │
-- └─────────────────────────────────────────┘

-- and its convenient `sequence` method?

newtype TravRec :: (Type -> Type) -> Row Type -> Type
newtype TravRec applic rowAp = TravRec (Record rowAp)

-- ┌───────────────────────────────────────────────────────────────────────────┐
-- │ Helper classes/instances (RecordOfAp, UnApRecord)                         │
-- ├───────────────────────────────────────────────────────────────────────────┤
-- │ RecordOfAp                                                                │
-- │   Record where every value type is in an Applicative @applic.             │
-- │                                                                           │
-- │ UnApRecord                                                                │
-- │   For RowLists @listAp and @listNoApp, if @applic @listAp has an instance │
-- │   of RecordOfAp, then @listNoAp is equivalent to @listAp with all value   │
-- │   types unwrapped from @applic.                                           │
-- └───────────────────────────────────────────────────────────────────────────┘

class RecordOfAp :: (Type -> Type) -> Type -> Constraint
class RecordOfAp applic rec

instance
  ( Applicative f
  , IsSymbol key
  , RowToList row (Cons key (f a) Nil)
  ) =>
  RecordOfAp f { | row }

else instance
  ( Applicative f
  , IsSymbol key
  , RowToList row (Cons key (f a) list)
  , RowToList row' list
  , RecordOfAp f { | row' }
  ) =>
  RecordOfAp f { | row }

class UnApRecord :: (Type -> Type) -> RowList Type -> RowList Type -> Constraint
class UnApRecord applic listAp listNoAp

instance Applicative f => UnApRecord f Nil Nil
instance
  ( Applicative f
  , UnApRecord f restAp restNoAp
  ) =>
  UnApRecord f (Cons key (f a) restAp) (Cons key a restNoAp)

-- ┌───────────────────────────────────────────────────┐
-- │ TraversableRecord class and instances for TravRec │
-- └───────────────────────────────────────────────────┘

class TraversableRecord
  :: ((Type -> Type) -> Row Type -> Type) -- applic, rowAp, newtype
  -> (Type -> Type) -- applic
  -> Row Type -- rowAp
  -> Row Type -- rowNoAp
  -> Constraint
class TraversableRecord newty applic rowAp rowNoAp where
  sequenceRecord :: newty applic rowAp -> applic (Record rowNoAp)

instance
  ( RowToList rowAp Nil
  , Applicative applic
  ) =>
  TraversableRecord TravRec applic rowAp rowAp where
  sequenceRecord :: TravRec applic rowAp -> applic (Record rowAp)
  sequenceRecord (TravRec rec) = pure rec

else instance
  ( RowToList rowAp (Cons key (applic val) listAp')
  , RowToList rowAp' listAp'
  , RowToList rowNoAp (Cons key val listNoAp')
  , RowToList rowNoAp' listNoAp'
  , Applicative applic
  -- , RecordOfAp applic { | rowAp }
  -- , UnApRecord applic listAp listNoAp
  , IsSymbol key
  , Cons key (applic val) rowAp' rowAp
  , Cons key val rowNoAp' rowNoAp
  , Lacks key rowAp'
  , Lacks key rowNoAp'
  , TraversableRecord TravRec applic rowAp' rowNoAp'
  ) =>
  TraversableRecord TravRec applic rowAp rowNoAp where
  sequenceRecord :: TravRec applic rowAp -> applic (Record rowNoAp)
  sequenceRecord (TravRec rec) =
    let
      key = Proxy @key :: Proxy key
      val = get key rec :: applic val
      rec' = delete key rec :: Record rowAp'
      trec' = TravRec rec' :: TravRec applic rowAp'
      seqRec' = sequenceRecord trec' :: applic (Record rowNoAp')
      ins = insert key <$> val :: applic (Record rowNoAp' -> Record rowNoAp)
      result = ins <*> seqRec' :: applic (Record rowNoAp)
    in
      result

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
