module Record.Traversable
  ( sequence
  , class TraversableRecord
  , sequenceRecord
  , TravRec
  , class RecordToRow
  , toRec
  , toRow
  ) where

import AdventOfCode.Prelude hiding (sequence)

import Data.Symbol (class IsSymbol)
import Record (delete, get, insert)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (class RowToList, Cons, Nil)

-- ┌───────────────────────────────────────────────────────────┐
-- │ TravRec: Newtype with instance of TraversableRecord class │
-- │ Convenience method "sequence":                            │
-- │   Use "sequenceRecord" without creating a TravRec         │
-- └───────────────────────────────────────────────────────────┘

newtype TravRec :: (Type -> Type) -> Row Type -> Type
newtype TravRec applic rowAp = TravRec (Record rowAp)

sequence
  :: forall f (r :: Row Type) (r' :: Row Type) (rec :: Type) (@rec' :: Type)
   . Applicative f
  => RecordToRow rec r
  => RecordToRow rec' r'
  => TraversableRecord TravRec f r r'
  => rec
  -> f rec'
sequence =
  map (toRec :: Record r' -> rec')
    <<< (sequenceRecord @TravRec @f @r @r' :: TravRec f r -> f (Record r'))
    <<< (TravRec :: Record r -> TravRec f r)
    <<< (toRow :: rec -> Record r)

-- ┌───────────────────────────────────────────────────────────┐
-- │ RecordToRow: Equates a Row Type, RowList Type, and Record │
-- └───────────────────────────────────────────────────────────┘

class RecordToRow :: Type -> Row Type -> Constraint
class RecordToRow rec row | rec -> row, row -> rec where
  toRow :: rec -> { | row }
  toRec :: { | row } -> rec

instance RecordToRow { | row } row where
  toRow = identity
  toRec = identity

-- ┌───────────────────────────────────────────────────┐
-- │ TraversableRecord: class and instance for TravRec │
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
