--
-- Day 12: Christmas Tree Farm
--
module Main where

import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad

main :: IO ()
main = do
  problems <- readInput <$> getContents
  putStrLn ("Number of problems: " ++ show (length problems))
  putStrLn ("Won't fit: " ++ show (length (filter wontFit problems)))
  putStrLn ("Will fit:  " ++ show (length (filter willFit problems)))  
                                 
    
type Coord = (Int,Int)
type Shape = Set Coord
type Region = (Int,Int)           -- width, height
type Problem = (Region, [Shape])  -- region and shapes to put inside
type Input = [Problem]

readInput :: String -> Input
readInput txt = problems
  where
    shapes =
      map readShape $
      take 6 $
      map (map snd)  $
      groupBy (\(i,_) (j,_) -> i`div`5==j`div`5)  (zip [0 ..] (lines txt))
    problems = map (readProblem shapes) (drop 30 (lines txt))

readShape :: [String] -> Shape
readShape (_:rest)  -- ignore the first line and last line
  = Set.fromList [(i,j) | (i,xs)<-zip [0..] (init rest)
                        , (j,x)<-zip [0..] xs, x=='#']

readProblem :: [Shape] -> String -> (Region,[Shape])
readProblem shapes txt = (region, list)
  where first:rest = words txt
        region = head [ (width,height)
                      | (width,'x':cont) <-reads first,
                        (height, ':':_) <- reads cont ]
        list = [ s | (i,n) <- zip [0..] (map read rest)
                   ,  s <- replicate n (shapes!!i) ]


-- detect problems that cannot possibly be fit
wontFit :: Problem -> Bool
wontFit ((width,height), shapes)
  = width*height < sum (map Set.size shapes)

-- detect problems that will definitely fit
willFit :: Problem -> Bool
willFit ((width,height), shapes)
  = (width`div`3)*(height`div`3) >= length shapes
  



