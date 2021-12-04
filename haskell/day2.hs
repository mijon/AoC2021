import Data.List

sample_data = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

-- part 1

type Direction = String
type Pinstruction = (Direction, Int)

parseInstruction :: String -> Pinstruction
parseInstruction s = (a, read b) where
   a:b:_ = words s

filterInstructions :: String -> [Pinstruction] -> [Pinstruction]
filterInstructions s ps = filter (checkDirection s) ps

checkDirection :: String -> Pinstruction -> Bool
checkDirection s' i = s' == s where
  (s, n) = i

getValue :: Pinstruction -> Int
getValue (s,i) = i

part1 :: [String] -> Int
part1 is = f * (d - u) where
  parsed = map parseInstruction is
  f = sum $ map getValue $ filterInstructions "forward" parsed
  d = sum $ map getValue $ filterInstructions "down" parsed
  u = sum $ map getValue $ filterInstructions "up" parsed







-- main
main :: IO ()
main = interact $ show . part1 . lines
