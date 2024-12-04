module AdventOfCode.Twenty24.Four
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
  input <- readTextFile UTF8 "./input/4"
  liftEffect do
    log "Part 1:"
    -- log ""
    -- logShow $ solve1 input
    log "Part 2:"
    -- log ""
    -- logShow $ solve2 input