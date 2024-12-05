module AdventOfCode.Twenty24.Five
  ( main
  ) where

import AdventOfCode.Prelude

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)
import Parsing.String.Basic (skipSpaces)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/5"
  liftEffect do
    log "Part 1:"
    -- logShow $ solve1 input
    log "Part 2:"
    -- logShow $ solve2 input
    log "End"

