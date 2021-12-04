import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Char

sample_data = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

-- Parser --
-- Using the Text.ParserCombinators.ReadP library
-- I'm going to make a basic parser for the instructions

-- We first make a record type for holding Instructions
data Instruction = Instruction
    { dir :: String
    , value :: Int
    }
    deriving Show

digit :: ReadP Char
digit = satisfy isDigit

numbers :: Int -> ReadP Int
numbers digits = 
    fmap read (count digits digit)

-- Since parser combinators are monads, we can correct the sign
-- of the value (i.e. set "up"s to negative values) while we are
-- parsing using this function
correctSign :: String -> Int -> Int
correctSign "up" n = -n
correctSign _ n = n

-- This is our parser for the Instructions
-- It takes us from Strings to Instructions.
instruction :: ReadP Instruction
instruction = do
    dir <- string "forward" <|> string "up" <|> string "down"
    string " "
    value <- numbers 1
    return (Instruction dir (correctSign dir value))

-- Since the result of mapping our parser over the data is of type
-- [[(Instruction, String)]], we need to pull out the successful 
-- Instruction data
getInstructions :: [String] -> [Instruction]
getInstructions ss = map (fst . head) (map (readP_to_S instruction) ss)

isDirection :: String -> Instruction -> Bool
isDirection s i = dir i == s

sumVals :: [Instruction] -> Int
sumVals = sum . map value

part1 :: [String] -> Int
part1 ss = f * (d + u) where
  is = getInstructions ss
  f = sumVals $ filter (isDirection "forward") is
  d = sumVals $ filter (isDirection "down") is
  u = sumVals $ filter (isDirection "up") is

main :: IO ()
main = interact $ show . part1 . lines 
