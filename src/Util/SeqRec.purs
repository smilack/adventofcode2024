module AdventOfCode.Twenty24.Util.SeqRec where

import AdventOfCode.Prelude
import Data.Symbol (class IsSymbol)
import Record (get, set, insert)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (Cons, Nil, class RowToList)

seqrec
  :: forall @l a r0 r1
   . IsSymbol l
  => Cons l (Maybe a) () r0
  => RowToList r0 (Cons l (Maybe a) Nil)
  => Cons l a () r1
  => Record r0
  -> Maybe (Record r1)
seqrec r0 = case get l r0 of
  Nothing -> Nothing
  Just val -> Just $ set l val r0
  where
  l = Proxy @l

seqadd
  :: forall @l a r0 r1 r2
   . IsSymbol l
  => Cons l (Maybe a) () r0
  => RowToList r0 (Cons l (Maybe a) Nil)
  => Lacks l r1
  => Cons l a r1 r2
  => Record r0
  -> Maybe (Record r1)
  -> Maybe (Record r2)
seqadd = flip (addseq @l)

addseq
  :: forall @l a r0 r1 r2
   . IsSymbol l
  => Cons l (Maybe a) () r0
  => RowToList r0 (Cons l (Maybe a) Nil)
  => Lacks l r1
  => Cons l a r1 r2
  => Maybe (Record r1)
  -> Record r0
  -> Maybe (Record r2)
addseq mr1 r0 = case mr1 of
  Nothing -> Nothing
  Just r1 -> case get l r0 of
    Nothing -> Nothing
    Just a -> Just (insert l a r1)
  where
  l = Proxy @l

infixl 5 addseq as <>?
