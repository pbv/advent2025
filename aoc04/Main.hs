{-# LANGUAGE BangPatterns #-}
--
-- Day 4:  Printing Department
--
module Main where
import qualified Data.Set as Set
import           Data.Set (Set)

main :: IO ()
main = do
  grid <- (readInput . lines) <$> getContents
  putStrLn "Part 1:"
  print (part1 grid)
  putStrLn "Part 2:"
  print (part2 grid)

type Grid = Set (Int,Int)

readInput :: [String] -> Grid
readInput xss
  = Set.fromList [(i,j) | (i,xs)<-zip [0..] xss
                        , (j,x)<-zip [0..] xs
                        , x=='@' ]

neighbours :: (Int,Int) -> Grid -> Int
neighbours (x,y) grid
  = length [ pos | pos<-[ (x-1,y), (x+1,y),
                          (x,y-1), (x,y+1),
                          (x-1,y-1), (x-1,y+1),
                          (x+1,y-1), (x+1,y+1) ]
                 , Set.member pos grid ]
  
-- part 1
part1 :: Grid -> Int
part1 grid = length [pos | pos<-Set.toList grid, neighbours pos grid<4 ]

-- part 2
part2 :: Grid -> Int
part2 = go 0
  where
  go !acc grid
    | null accessible = acc
    | otherwise = go (acc+length accessible) grid'        
    where accessible = [pos | pos<-Set.toList grid, neighbours pos grid<4 ]
          grid' = foldr Set.delete grid accessible
    


    
