module Record.Foldable where

import AdventOfCode.Prelude hiding (sequence)

import Data.Symbol (class IsSymbol)
import Debug (traceM)
import Prim.RowList (RowList)
import Record (get, insert, set)
import Record.Traversable (sequence, class RecordToRow)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (class ListToRow, class RowToList, Cons, Nil)

-- | Proxy type to hold RowLists
data RowListProxy :: RowList Type -> Type
data RowListProxy rlpT = RowListProxy

-- | For some Applicative `ap`, ensure that every value in a Record (specified
-- | as a proxy with a RowList Type parameter) has type `ap a`.
class RecordOfAp :: (Type -> Type) -> Type -> Constraint
class RecordOfAp ap rlApV

instance recordOfApNil :: (Applicative f) => RecordOfAp f (RowListProxy Nil)

instance recordOfApCons ::
  ( Applicative ap
  , IsSymbol k
  , RecordOfAp ap (RowListProxy rlApT)
  ) =>
  RecordOfAp ap (RowListProxy (Cons k (ap v) rlApT))

-- | For some Applicative `ap`, ensure that two Records (specified as a proxies
-- | with RowList Type parameters) have the same keys and that for each value
-- | of type `a` in `rlpUnV`, the corresponding value in `rlpApV` has type
-- | `ap a`.
class UnApRecord :: (Type -> Type) -> Type -> Type -> Constraint
class UnApRecord ap rlpApV rlpUnV | ap rlpUnV -> rlpApV, ap rlpApV -> rlpUnV

instance unApRecordNil ::
  ( Applicative f
  ) =>
  UnApRecord ap (RowListProxy Nil) (RowListProxy Nil)

instance unApRecordCons ::
  ( Applicative ap
  , IsSymbol k
  , UnApRecord ap (RowListProxy rlpApT) (RowListProxy rlpUnT)
  ) =>
  UnApRecord ap
    (RowListProxy (Cons k (ap a) rlpApT))
    (RowListProxy (Cons k a rlpUnT))

-- | Unwrap a `RowListProxy rlpT` and convert `rlpT` to a Row Type.
class ListProxyToRow :: Type -> Row Type -> Constraint
class ListProxyToRow rlpV row | row -> rlpV, rlpV -> row

instance (RowToList row rlpT) => ListProxyToRow (RowListProxy rlpT) row

-- | Wrapper to allow writing instances for records.
newtype SeqRec :: Type -> Type
newtype SeqRec recAp = SeqRec recAp

-- | Constraints allowing sequencing a record held by a newtype.
class SequenceableRecord :: (Type -> Type) -> Type -> Type -> Constraint
class SequenceableRecord t rlpApV rowNoAp where
  sequenceImpl
    :: forall ap rowAp rowUn rlpUnT
     . Applicative ap
    => ListProxyToRow rlpApV rowAp
    => RecordOfAp ap rlpApV
    => UnApRecord ap rlpApV (RowListProxy rlpUnT)
    => ListToRow rlpUnT rowUn
    => rlpApV
    -> t (Record rowAp)
    -> ap (Record rowUn)

