--
-- Day 8: Playground
--
module Main where

import Data.List
import Data.Function(on)
import Data.Graph
import Data.Tree

main :: IO ()
main = do
  input <- readInput <$> getContents
  putStrLn "Part 1"
  print (part1 input)
  putStrLn "Part 2"
  print (part2 input)

type Input = [Coord]

data Coord = Coord !Int !Int !Int !Int -- id, x, y, z
  deriving Show

readInput :: String -> Input
readInput txt = [readCoord i xs | (i,xs)<-zip [0..] (lines txt)]

readCoord :: Int -> String -> Coord
readCoord id txt = let (x,y,z) = read ("(" ++ txt ++ ")")
                   in Coord id x y z

-- Part 1
part1 :: Input -> Int
part1 = solve1 . makeGraph 1000 

solve1 :: Graph -> Int
solve1 = product . take 3 . reverse . sort . map length . dff 

makeGraph :: Int -> Input -> Graph
makeGraph limit coords = buildG (0,length coords-1) (edges++edges')
  where pairs = sortBy (compare`on`uncurry sqDistance)
                [(v, v') | (v:vs)<-tails coords, v'<-vs]
        edges = [(identifier v, identifier v') | (v,v')<-take limit pairs]
        edges'= map (\(x,y)->(y,x)) edges
        

       
-- get the identifier
identifier :: Coord -> Int
identifier (Coord i _ _ _) = i


-- compute the squared distance
sqDistance :: Coord -> Coord -> Int
sqDistance (Coord _ x1 y1 z1) (Coord _ x2 y2 z2)
  = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2
  
-- Part 2; inefficent solution because I'm lazy
part2 :: Input -> Int
part2 coords = x1*x2
  where
    pairs = sortBy (compare`on`uncurry sqDistance)
            [(v, v') | (v:vs)<-tails coords, v'<-vs]
    ((Coord _ x1 _ _ ,Coord _ x2 _ _): _) = drop (k-1) pairs
    k = go 0 ((length coords)^2`div`2)
    go lo hi
      | lo >= hi = lo
      | otherwise =
              let
                limit = (hi+lo)`div`2
                g = makeGraph limit coords
              in if single (components g) then
                   go lo (limit-1)
                 else
                   go (limit+1) hi

single :: [a] -> Bool
single [_] = True
single _  = False
