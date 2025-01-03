module Record.Foldable where

import AdventOfCode.Prelude

import Data.Symbol (class IsSymbol)
import Debug (traceM)
import Prim.RowList (RowList)
import Record (get, insert, set)
import Record.Traversable (sequence, class RecordToRow)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (class ListToRow, class RowToList, Cons, Nil)

-- newtype Rec row list = Rec { | row }

data RowListProxy :: RowList Type -> Type
data RowListProxy rlpT = RowListProxy

class RecordOfAp :: (Type -> Type) -> Type -> Constraint
class RecordOfAp applic rlpApT

instance (Applicative f) => RecordOfAp f (RowListProxy Nil)

instance
  ( Applicative f
  , IsSymbol k
  , RecordOfAp f (RowListProxy tail)
  ) =>
  RecordOfAp f (RowListProxy (Cons k (f v) tail))

class UnApRecord :: (Type -> Type) -> Type -> Type -> Constraint
class UnApRecord applic rlpApV rlpUnV | applic rlpUnV -> rlpApV

instance
  ( Applicative f
  
instance
  ( Applicative f
  , RowToList row rlp
  ) =>
  UnApRecord f (RowListProxy rlp) row

newtype SeqRec :: (Type -> Type) -> Row Type -> Type
newtype SeqRec applic rowAp = SeqRec { | rowAp }

class SequenceableRecord :: Type -> Type -> Type -> Constraint
class SequenceableRecord t rlp rowNoAp where
  sequenceImpl
    :: forall f rowAp
     . Applicative f
    => RecordOfAp f rlp
    => UnApRecord f rlp rowAp
    => t f rowAp
    -> f { | rowNoAp }

-- class TraversableRecord newty applic rowAp rowNoAp where
--   sequenceRecord :: newty applic rowAp -> applic (Record rowNoAp)
