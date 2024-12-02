module AdventOfCode.Twenty24.Two
  ( main
  ) where

import Prelude
import AdventOfCode.Twenty24.Util

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/2"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part2:"
    -- log ""
    -- logShow $ solve2 input