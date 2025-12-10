--
-- Day 5: Caffeteria
--
module Main where
import Data.List (sort)

type Input = ([Range], [Int])
type Range = (Int,Int)

main :: IO ()
main = do
  inp <- readInput <$> getContents
  putStrLn "Part 1"
  print (part1 inp)
  putStrLn "Part 2"
  print (part2 inp)

readInput :: String -> ([Range], [Int])
readInput txt = (map parseRange ls',  map read ls'')
  where
    ls = lines txt
    ls'  = takeWhile (not.null) ls
    ls'' = tail $ dropWhile (not.null) ls

parseRange :: String -> (Int,Int)
parseRange xs = head [ (a,b) | (a, '-':xs')<-reads xs, (b,"")<-reads xs']

-- Part 1
part1 :: Input -> Int
part1 (ranges, ids) = length [id | id<-ids, any (inRange id) ranges]

inRange :: Int -> Range -> Bool
inRange x (a,b) = a<=x && x<=b

-- Part 2
part2 :: Input -> Int
part2 (ranges, _) = sum [b-a+1 | (a,b)<-join (sort ranges)]


-- join overlaping ranges; assumes ranges are sorted lexicographically 
join :: [Range] -> [Range]
join ((a,b):(c,d):rest)
  | a<=c && c<=b = join ((a,max b d):rest)
  | otherwise = (a,b):join ((c,d):rest)
join rest = rest



    
