module AdventOfCode.Twenty24.Three
  ( compute
  , main
  , parse
  , solve1
  ) where

import AdventOfCode.Twenty24.Util
import Prelude

import Data.Either (fromRight)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (between, many, optional, try)
import Parsing.String (anyTill, string)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/3"
  liftEffect do
    log "Part 1:"
    logShow $ solve1 input
    log "Part 2:"

-- log ""
-- logShow $ solve2 input

solve1 :: String -> Int
solve1 = compute <<< parse

compute :: List (Tuple Int Int) -> Int
compute = sumMap $ uncurry (*)

parse :: String -> List (Tuple Int Int)
parse s = fromRight Nil $ runParser s sift

sift :: Parser String (List (Tuple Int Int))
sift = many $ try nextMul

nextMul :: Parser String (Tuple Int Int)
nextMul = snd <$> anyTill mul

mul :: Parser String (Tuple Int Int)
mul = between (string "mul(") (string ")") operands

operands :: Parser String (Tuple Int Int)
operands = do
  a <- intDecimal
  _ <- string ","
  b <- intDecimal
  pure $ a /\ b