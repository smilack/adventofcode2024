module Record.Traversable where

import AdventOfCode.Prelude hiding (sequence)

import Data.Symbol (class IsSymbol)
import Record (delete, get, insert, set)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, class Cons)
import Type.RowList (RowList, class RowToList, class ListToRow, Cons, Nil)