--
-- Day 2:  Gift shop
--
module Main where

main :: IO ()
main = do
  txt <- getContents
  let ranges = readInput txt
  putStrLn "Part 1:"
  print (part1 ranges)
  putStrLn "Part 2:"
  print (part2 ranges)


readInput :: String -> [(Int,Int)]
readInput
  = map readRange .
    words .
    map (\x -> if x==',' then ' ' else x) 
  
readRange :: String -> (Int,Int)
readRange xs =
  head [ (a,b) | (a,'-':xs')<-reads xs, (b, "")<-reads xs' ]

{-
-- all invalid numbers with n digits
invalid :: Int -> [Int]
invalid n
  | even n = map dup [10^(k-1) .. 10^k-1]
  | otherwise = []
  where k = n`div`2
        dup x = x*10^k + x

rangeInvalid :: (Int,Int) -> [Int]
rangeInvalid (lo, hi)
  = filter (\x -> x>=lo && x<=hi) $  concatMap invalid [n..m]
  where
    n = length (show lo)
    m = length (show hi)
-}


-- part 1
invalid1 :: Int -> Bool
invalid1 n = n`div`(10^k) == n`mod`(10^k)
  where k = length (show n)`div`2


part1 :: [(Int,Int)] -> Int
--part1 = sum . concatMap rangeInvalid
part1 ranges = sum [n | (lo,hi)<-ranges, n<-[lo..hi], invalid1 n]


-- part 2
invalid2 :: Int -> Bool
invalid2 n
  = any allEqual [chunks n (10^i) | i<-[1..k`div`2], k`mod`i == 0]
  where k = length (show n)
        

allEqual :: [Int] -> Bool
allEqual (x:xs) = all (==x) xs


chunks :: Int -> Int -> [Int]
chunks num k = go num
  where go n
          | n>0 = (n`mod`k) : go (n`div`k) 
          | otherwise = []
      

part2 :: [(Int,Int)] -> Int
part2 ranges
  = sum [n | (lo,hi)<-ranges, n<-[lo..hi], invalid2 n]
