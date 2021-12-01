-- Day 1
import Data.List

sample_data :: [Int]
sample_data = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

-- part 1
part1 :: [Int] -> Int
part1 ns = length $ filter (>0) $ map signum $ zipWith (-) (tail ns) ns

-- part 2 
window3 :: [a] -> [[a]]
window3 (x:xs) | length xs > 1 = take 3 (x:xs) : window3 xs
               | otherwise = []

part2 :: [Int] -> Int
part2 ns = length $ filter (>0) $ map signum $ map sum $ window3 $ zipWith (-) (tail ns) ns 

main :: IO ()
main = interact $ show . part2 . map read . tail . lines
