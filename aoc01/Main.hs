{-# LANGUAGE BangPatterns #-}
--
-- Day 1:  Secret Entrance
--
module Main where
import Data.List
import Test.QuickCheck 



main :: IO ()
main = do
  rots <- readInput <$> getContents
  putStrLn "Part 1:"
  print (part1 rots)
  putStrLn "Part 2:"
  print (part2 rots)


readInput :: String -> [Int]
readInput = map readTurn . lines
  
readTurn :: String -> Int
readTurn ('R':rest) = read rest
readTurn ('L':rest) = negate (read rest)
readTurn _ = error "invalid input"

-- part 1
part1 :: [Int] -> Int
part1 turns = length $ filter (==0) headings
  where
    headings = scanl' turn 50 turns
    turn curr x = (curr + x) `mod` 100

-- part 2
-- Brute force
part2'spec :: [Int] -> Int
part2'spec turns = length $ filter (==0) headings
  where
    headings = foldl' rotate [50] turns

rotate :: [Int] -> Int -> [Int]
rotate acc@(!h:_) x
  | x>0 = rotate (((h+1)`mod`100) : acc) (x-1)
  | x<0 = rotate (((h-1)`mod`100) : acc) (x+1)
  | otherwise = acc


-- "Smart" version
part2 :: [Int] -> Int
part2  turns = zeros + extra - zeros2
  where
    zeros = length $ filter (==0) headings
    -- remove consecutive zeros (counted twice)
    zeros2 = length $
             filter (\(h,h') -> h==0 && h'==0) $
             zip headings (tail headings)
    extra = sum [correct h t | (h,t)<-zip headings turns] 
    headings = scanl' turn 50 turns
    turn h x = (h+x)`mod`100

correct :: Int -> Int -> Int
correct h t
  | t>0 = q + if h>0 && h+r>100 then 1 else 0
  | t<0 = q + if h>0 && h-r<0 then 1 else 0
  | otherwise = 0
  where (q,r) = divMod (abs t) 100

--
-- QuickCheck tests
--
prop_part2_correct :: Property
prop_part2_correct 
  = forAllShrink arbitrary myShrinker $
   \xs -> part2 xs == part2'spec xs

-- shrinker for rotations
myShrinker :: [Int] -> [[Int]]
myShrinker xs
  = [  prefix ++ (x+x'):rest
    | (prefix,x:x':rest) <- zip (inits xs) (tails xs)
    ]
    ++
    shrinkList (\x -> [x-100 | x>=100] ++ [x+100 | x<= -100]) xs

