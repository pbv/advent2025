--
-- Day 9: Movie Theather
--
module Main where

import Data.List

main :: IO ()
main = do
  input <- readInput <$> getContents
  putStrLn "Part 1"
  print (part1 input)
  putStrLn "Part 2"
  print (part2 input)


type Pos = (Int,Int)

readInput :: String -> [Pos]
readInput = map readPos . lines

readPos :: String -> Pos
readPos txt = read ("("++txt++")")


-- Part 1
part1 :: [Pos] -> Int
part1 coords
  = maximum [rectArea p p' | (p:ps) <- tails coords, p'<-ps]

rectArea :: Pos -> Pos -> Int
rectArea (x1,y1) (x2,y2) = (1+abs (x1-x2))*(1+abs (y1-y2))


-- Part 2
part2 :: [Pos] -> Int
part2 coords
  = maximum [rectArea p p'
            | (p:ps) <- tails coords, p'<-ps, checkRect p p' segs
            ]
  where
    segs = segments coords

checkRect :: Pos -> Pos  -> [Line] -> Bool
checkRect (x1,y1) (x2,y2) segs
  = all (inside segs) [(x1,y2), (x2,y1)]
    &&
    and [ not (crosses seg seg') | seg <- segs
                                , seg'<- edges]
  where edges = [ ((x1,y1), (x1,y2))
                , ((x2,y1), (x2,y2))
                , ((x1,y1), (x2,y1))
                , ((x1,y2), (x2,y2))
                ]


-- check if two orthogonal lines cross each other
crosses :: Line -> Line -> Bool
crosses seg1 seg2
  = vhcross seg1 seg2 || vhcross seg2 seg1
  where
    -- vertical-horizontal strict crossing    
    vhcross ((x1,y1),(x2,y2)) ((x1',y1'),(x2',y2'))
      = x1==x2 && y1'==y2' &&
        min x1' x2'< x1 && x1 < max x1' x2' &&
        min y1 y2 < y1' && y1'< max y1 y2 



      


type Line = (Pos,Pos)  -- line segment

segments :: [Pos] -> [Line]
segments (p:ps) = zip (p:ps) (ps++[p])

-- intersections starting from a point and
-- moving in increasing x,y coordinates
intersects :: Pos -> Line -> Bool
intersects (x,y) ((x1,y1), (x2,y2))
  | x1==x2  -- vertical line
    = x<=min x1 x2 && smin <= x-y && x-y < smax 
  | y1==y2  -- horizontal line
    = y<=y1 && smin <= x-y && x-y < smax
  | otherwise = error "invalid line segment"
  where smax = max (x1-y1) (x2-y2)
        smin = min (x1-y1) (x2-y2)
        

contains :: Pos -> Line -> Bool
contains (x,y) ((x1,y1), (x2,y2))
  | x1 == x2
    -- vertical line
    = x==x1 && min y1 y2<= y && y <= max y1 y2
  | y1 == y2
    -- horizontal line
    = y==y1 && min x1 x2 <= x && x <= max x1 x2
  | otherwise = error "invalid line"

inside :: [Line] -> Pos -> Bool
inside segments pos
  = any (contains pos) segments
    ||
    odd (length [seg | seg<-segments, intersects pos seg]) 


prettyPrint :: FilePath -> IO ()
prettyPrint path = do
  ps <- readInput <$> readFile path
  prettyAux ps

prettyAux :: [Pos] -> IO ()
prettyAux ps =
  putStrLn $ unlines [[charAt x y | x<-[xmin..xmax]] | y<-[ymin..ymax]]
  where
    poly = segments ps
    xmin = minimum (map fst ps) - 1
    xmax = maximum (map fst ps) + 1
    ymin = minimum (map snd ps) - 1
    ymax = maximum (map snd ps) + 1
    charAt x y
      | (x,y) `elem`ps    = '#'
      | inside poly (x,y) = 'X'
      | otherwise         = '.'
