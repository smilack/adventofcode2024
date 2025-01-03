module Record.Foldable where

import AdventOfCode.Prelude hiding (sequence)

import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Debug (traceM)
import Prim.RowList (RowList)
import Record (get, insert, set)
import Record.Traversable (sequence, class RecordToRow)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (class ListToRow, class RowToList, Cons, Nil)

-- | Proxy type to hold RowLists
-- data R_owListProxy :: RowList Type -> Type
-- data R_owListProxy rlpT = R_owListProxy

-- | For some Applicative `ap`, ensure that every value in a Record (specified
-- | as a proxy with a RowList Type parameter) has type `ap a`.
class RecordOfAp :: (Type -> Type) -> Type -> Constraint
class RecordOfAp ap rlApV

instance recordOfApNil :: (Applicative f) => RecordOfAp f (Proxy Nil)

instance recordOfApCons ::
  ( Applicative ap
  , IsSymbol k
  , RecordOfAp ap (Proxy rlApT)
  ) =>
  RecordOfAp ap (Proxy (Cons k (ap v) rlApT))

-- | For some Applicative `ap`, ensure that two Records (specified as a proxies
-- | with RowList Type parameters) have the same keys and that for each value
-- | of type `a` in `rlpUnV`, the corresponding value in `rlpApV` has type
-- | `ap a`.
class UnApRecord :: (Type -> Type) -> Type -> Type -> Constraint
class UnApRecord ap rlpApV rlpUnV | ap rlpUnV -> rlpApV, ap rlpApV -> rlpUnV

instance unApRecordNil ::
  ( Applicative f
  ) =>
  UnApRecord ap (Proxy Nil) (Proxy Nil)

instance unApRecordCons ::
  ( Applicative ap
  , IsSymbol k
  , UnApRecord ap (Proxy rlpApT) (Proxy rlpUnT)
  ) =>
  UnApRecord ap
    (Proxy (Cons k (ap a) rlpApT))
    (Proxy (Cons k a rlpUnT))

-- | Unwrap a `Proxy rlpT` and convert `rlpT` to a Row Type.
class RowToListProxy :: Type -> RowList Type -> Row Type -> Constraint
class RowToListProxy p rl row where

-- ltor
--   :: forall
--    . 
--   => Proxy 
--   -> row
--   -> row

instance (RowToList row rl) => RowToListProxy (Proxy rl) rl row

-- | Wrapper to allow writing instances for records.
newtype SeqRec :: Type -> Type
newtype SeqRec recAp = SeqRec recAp

derive instance Newtype (SeqRec a) _
{- 
-- | Constraints allowing sequencing a record held by a newtype.
class SequenceableRecord :: (Type -> Type) -> RowList Type -> Constraint
class SequenceableRecord t rlAp where
  sequenceImpl
    :: forall ap rowAp rowUn rlUn
     . Applicative ap
    => ListProxyToRow (Proxy rlAp) rowAp
    => Newtype (t (Record rowAp)) (Record rowAp)
    => RecordOfAp ap (Proxy rlAp)
    => UnApRecord ap (Proxy rlAp) (Proxy rlUn)
    => ListToRow rlUn rowUn
    => Proxy rlAp
    -> t (Record rowAp)
    -> ap (Record rowUn)

instance sequenceableRecordNil :: SequenceableRecord t Nil where
  sequenceImpl
    :: forall ap rowAp rowUn
     . Applicative ap
    => ListProxyToRow (Proxy Nil) rowAp
    => Newtype (t (Record rowAp)) (Record rowAp)
    => RecordOfAp ap (Proxy Nil)
    => UnApRecord ap (Proxy Nil) (Proxy Nil)
    => ListToRow Nil rowUn
    => Proxy Nil
    -> t (Record rowAp)
    -> ap (Record rowUn)
  sequenceImpl _ t = pure recUn
    where
    recAp = (unwrap t) :: Record rowAp


    recUn = () :: Record rowUn -}