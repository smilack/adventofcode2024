module AdventOfCode.Twenty24.Util.SeqRec where

import AdventOfCode.Prelude

import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol)
import Debug (spy)
import Prim.RowList (RowList)
import Record (delete, get, insert, set)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (Cons, Nil, class RowToList)

main :: Effect Unit
main = do
  log "SeqRec testing"
  --
  -- logShow x
  --
  log "End"

{-
x :: Sequence State
x = Sequence { dir: "asdf", lab: 3, pos: true }

type State =
  { dir :: String
  , lab :: Int
  , pos :: Boolean
  }

type StateList =
  (Cons "dir" String (Cons "lab" Int (Cons "pos" Boolean Nil)))
-}

-- ┌──────────────┐
-- │ SeqRec class │
-- └──────────────┘

{-
-- TODO: can add a function `seqrec` or whatever that only accepts a
--   `RecordOfMaybes` and creates the `Sequence`/runs `sequence` or
--   `mapFromMaybes` or whatever

-- class RecordOfMaybes: Record where every `value :: Maybe a`
class RecordOfMaybes :: RowList Type -> Constraint
class RecordOfMaybes list

instance RecordOfMaybes Nil
instance RecordOfMaybes rest => RecordOfMaybes (Cons l (Maybe a) rest)

-- class MaybesToStuff: Check that each `value :: Maybe a` in `listm` has a
--   corresponding `value :: a` in `list`
class MaybesToStuff :: RowList Type -> RowList Type -> Constraint
class MaybesToStuff listm list

instance MaybesToStuff Nil Nil
instance
  ( MaybesToStuff restm rest
  ) =>
  MaybesToStuff (Cons l (Maybe a) restm) (Cons l a rest)
-}

class RecordOfAp :: (Type -> Type) -> RowList Type -> Constraint
class RecordOfAp applic listAp

instance RecordOfAp Maybe Nil
instance RecordOfAp Maybe restAp => RecordOfAp Maybe (Cons key (Maybe a) restAp)

class UnApRecord :: (Type -> Type) -> RowList Type -> RowList Type -> Constraint
class UnApRecord applic listAp listNoAp

instance UnApRecord Maybe Nil Nil
instance
  UnApRecord Maybe restAp restNoAp =>
  UnApRecord Maybe (Cons key (Maybe a) restAp) (Cons key a restNoAp)

class TraversableRecord
  :: ((Type -> Type) -> Row Type -> Type) -- applic, rowAp, newtype
  -> (Type -> Type) -- applic
  -> Row Type -- rowAp
  -> Row Type -- rowNoAp
  -> Constraint
class TraversableRecord newty applic rowAp rowNoAp | rowAp -> rowNoAp where
  sequence
    :: forall listAp listNoAp
     . RowToList rowAp listAp
    => RowToList rowNoAp listNoAp
    => RecordOfAp applic listAp
    => UnApRecord applic listAp listNoAp
    => newty applic rowAp
    -> applic (Record rowNoAp)

instance TraversableRecord TravRec Maybe rowAp _ where
  sequence
    :: forall listAp listNoAp rowNoAp
     . RowToList rowAp listAp
    => RowToList rowNoAp listNoAp
    => RecordOfAp Maybe listAp
    => UnApRecord Maybe listAp listNoAp
    => TravRec Maybe rowAp
    -> Maybe (Record rowNoAp)
  sequence = -- I hope this works!

-- class SeqRec: Class that will have the `seqrec` function
--   (replaces `Show` in the example)

{-
class SeqRec :: (Type -> Type) -> Row Type -> Row Type -> Constraint
class SeqRec f rm r where
  sequence
    :: forall lm l
     . RowToList rm lm
    => RowToList r l
    => RecordOfMaybes lm
    => MaybesToStuff lm l
    => f (Record rm)
    -> Maybe (Record r)
-}

-- TODO: Can I make Maybe a ghost type of Sequence so I only need one Sequence
--   newtype? e.g.
--
--     newtype Sequence :: (Type -> Type) -> Type -> Type
--     newtype Sequence applic recAp = Sequence recAp
--
--     instance (...) => TraversableRecord (newty applic) rowAp rowNoAp where
--       ...
--
--   also do I need to make rowNoAp a parameter of TraversableRecord or can
--     I make it using constraints?

newtype Sequence :: Type -> Type
newtype Sequence r = Sequence r

newtype TravRec :: (Type -> Type) -> Row Type -> Type
newtype TravRec applic rowAp = TravRec (Record rowAp)

type FooMaybe = { foo :: Maybe Int }
type MaybeFoo = Maybe { foo :: Int }

-- pretty sure above this is fine

{- 
instance
  ( RowToList rowAp listAp
  , RowToList rowNoAp listNoAp
  , RecordOfAp Maybe listAp
  , UnApRecord Maybe listAp listNoAp
  ) =>
  TraversableRecord Sequence Maybe rowAp rowNoAp
  where
  sequence (Sequence record) = Nothing

instance
  ( RowToList rm lm
  , RowToList r l
  , RecordOfMaybes lm
  , MaybesToStuff lm l
  , Cons k (Maybe v) rm' rm
  , Cons k v r' r
  ) =>
  SeqRec Sequence rm r where
  sequence (Sequence record) = go record
  -- I guess I can't use where here, so it *has* to be separate
  where
  go :: Record rm -> Maybe (Record r)
  go {} = Just {}
  go rec = Nothing
-}

-- Can I make this a separate function so I don't have to keep wrapping and unwrapping Sequence?
-- The Cons constraints might be necessary for annotating the record functions and recursive step
-- mV = get k rec
-- rec' :: Record = delete k rec
-- insert k <$> mV <*> get (sequence @Sequence @rm' @r' $ Sequence $ rec')

-- do I need the below?

{-
class MapRow :: Row Type -> Row Type -> Constraint
class MapRow rm r where
  mapFromMaybe :: Record rm -> Maybe (Record r)

instance MapRow () () where
  mapFromMaybe _ = {}

instance
  ( Reflectable sym String
  , MapRow r' rest
  , IsSymbol sym
  , Cons sym k r' r
  , RowToList r (Cons sym k rest)
  , Lacks sym r'
  ) =>
  MapRow r (Cons sym k rest)
  where
  mapFromMaybe :: Record r -> List String
  mapFromMaybe rec = (Cons :: String -> List String -> List String) thisKey
    remainingKeys
    where
    thisKey = reflectType (Proxy @sym)

    _ = spy "get thisKey rec" $ get (Proxy @sym) rec

    restRec = delete (Proxy @sym) rec

    remainingKeys = mapFromMaybe @r' @rest restRec
 -}
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
