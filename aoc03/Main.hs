--
-- Day 3:  Lobby
--
module Main where
import Data.Char (ord)
import Data.List(foldl', maximumBy, init)

main :: IO ()
main = do
  jolts <- readInput <$> getContents
  putStrLn "Part 1:"
  print (part1 jolts)
  putStrLn "Part 2:"
  print (part2 jolts)


readInput :: String -> [[Int]]
readInput = map readJolts . lines

readJolts :: String -> [Int]
readJolts = map (\x -> ord x - ord '0')

--- Part 1
maxJolts :: [Int] -> Int
maxJolts xs = hi*10+lo
  where
    (hi, k) = maximumBy cmp (zip (init xs) [0..])
    lo = maximum (drop (k+1) xs)
    cmp (x,i) (y,j) = compare x y <> compare j i


part1 :: [[Int]] -> Int
part1 = sum . map maxJolts 
  
-- Part 2
maxJolts2 :: Int -> [Int] -> Int
maxJolts2 n xs = foldl' (\acc x -> acc*10+x) 0 (maxJoltsAux n xs)

maxJoltsAux :: Int -> [Int] -> [Int]
maxJoltsAux n xs
  | n>0 = hi : maxJoltsAux (n-1) xs'
  | otherwise = []
  where
    (hi, k) = maximumBy cmp (zip xs [0..length xs - n])
    cmp (x,i) (y,j) = compare x y <> compare j i
    xs' = drop (k+1) xs
    
part2 :: [[Int]] -> Int
part2 = sum . map (maxJolts2 12)
