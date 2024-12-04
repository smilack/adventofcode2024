module AdventOfCode.Twenty24.Three
  ( compute
  , filterEnabled
  , main
  , parse
  , solve1
  , solve2
  ) where

import AdventOfCode.Twenty24.Util (sumMap)
import Prelude (Unit, bind, discard, (<<<), ($), (<$>), (<>), (*), map, pure)

import Data.Either (fromRight)
import Data.List (List(..))
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (between, many, try, (<|>))
import Parsing.String (anyTill, rest, string)
import Parsing.String.Basic (intDecimal)

main :: Effect Unit
main = launchAff_ do
  input <- readTextFile UTF8 "./input/3"
  liftEffect do
    log "Part 1:"
    logShow $ solve1 input
    log "Part 2:"
    logShow $ solve2 input

solve1 :: String -> Int
solve1 = compute <<< parse

compute :: List (Tuple Int Int) -> Int
compute = sumMap $ uncurry (*)

parse :: String -> List (Tuple Int Int)
parse s = fromRight Nil $ runParser s sift

sift :: Parser String (List (Tuple Int Int))
sift = many $ try nextMul

nextMul :: Parser String (Tuple Int Int)
nextMul = snd <$> anyTill mulInst

mulInst :: Parser String (Tuple Int Int)
mulInst = between (string "mul(") (string ")") operands

operands :: Parser String (Tuple Int Int)
operands = do
  a <- intDecimal
  _ <- string ","
  b <- intDecimal
  pure $ a /\ b

solve2 :: String -> Int
solve2 = solve1 <<< filterEnabled

filterEnabled :: String -> String
filterEnabled s = fromRight "" $ runParser s $ sifter "" "do()"

sifter :: String -> String -> Parser String String
sifter segments cmd = try goSift <|> map (segments <> _) rest
  where
  goSift = do
    Tuple seg cmd' <- anyTill flag
    case cmd of
      "do()" -> sifter (segments <> seg) cmd'
      _ -> sifter segments cmd'

flag :: Parser String String
flag = string "do()" <|> string "don't()"
