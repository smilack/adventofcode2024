module AdventOfCode.Twenty24.Util.SeqRec where

import AdventOfCode.Prelude
import Data.Symbol (class IsSymbol)
import Record (get, set, insert)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (Cons, Nil, class RowToList)

main :: Effect Unit
main = do
  log "SeqRec testing"
  --
  log "End"

-- -- ┌──────────────┐
-- -- │ SeqRec class │
-- -- └──────────────┘

-- newtype ShowKeysOnly :: Row Type -> RL.RowList Type -> Type
-- newtype ShowKeysOnly r l = ShowKeysOnly (Record r)

-- class ShowKeysInRowList2 :: Row Type -> RL.RowList Type -> Constraint
-- class RL.RowToList r l <= ShowKeysInRowList2 r l | l -> r where
--   buildKeyList2 :: Record r -> List String

-- instance ShowKeysInRowList2 () RL.Nil where
--   buildKeyList2 _ = Nil

-- instance
--   ( Reflectable sym String
--   , ShowKeysInRowList2 r' rest
--   , IsSymbol sym
--   , Cons sym k r' r
--   , RL.RowToList r (RL.Cons sym k rest)
--   , Lacks sym r'
--   ) =>
--   ShowKeysInRowList2 r (RL.Cons sym k rest)
--   where
--   buildKeyList2 :: Record r -> List String
--   buildKeyList2 rec = (Cons :: String -> List String -> List String) thisKey
--     remainingKeys
--     where
--     thisKey = reflectType (Proxy @sym)

--     _ = spy "get thisKey rec" $ get (Proxy @sym) rec

--     restRec = delete (Proxy @sym) rec

--     remainingKeys = buildKeyList2 @r' @rest restRec

-- instance
--   ( ShowKeysInRowList2 recordRows rowList
--   ) =>
--   Show (ShowKeysOnly recordRows rowList)
--   where
--   show (ShowKeysOnly rec) = show $ buildKeyList2 @recordRows @rowList rec

-- type State =
--   ( dir :: String
--   , lab :: Int
--   , pos :: Boolean
--   )

-- type StateList =
--   (RL.Cons "dir" String (RL.Cons "lab" Int (RL.Cons "pos" Boolean RL.Nil)))

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

seqadd
  :: forall k v r0 r1 r2
   . IsSymbol k
  => Cons k (Maybe v) () r0
  => RowToList r0 (Cons k (Maybe v) Nil)
  => Lacks k r1
  => Cons k v r1 r2
  => Record r0
  -> Maybe (Record r1)
  -> Maybe (Record r2)
seqadd = flip addseq

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
