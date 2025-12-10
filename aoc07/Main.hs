{-# LANGUAGE RecordWildCards #-}
--
-- Day 7: Laboratories
--

module Main where

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map (Map)
import qualified Data.Map as Map


main :: IO ()
main = do
  input <- readInput <$> getContents
  putStrLn "Part 1"
  print (part1 input)
  putStrLn "Part 2"
  print (part2 input)

type Pos = (Int,Int)

data Input
  = Input { height :: Int
          , start :: Pos
          , splitters :: Set Pos
          }
  deriving Show


readInput :: String -> Input  
readInput txt = Input { height = h
                      , start = s
                      , splitters = pos
                  }
  where ls = lines txt
        h = length ls
        pos = Set.fromList [(i,j)
                           | (i,xs)<-zip [0..] ls,
                             (j,x)<-zip [0..] xs,
                             x == '^' ]
        s = head [(i,j)
                 | (i,xs)<-zip [0..] ls,
                   (j,x)<-zip [0..] xs,
                   x == 'S' ]


-- Part 1
part1 :: Input -> Int
part1 Input{..} = Set.size (dfs start Set.empty)
  where
    dfs (row,col) visited
      | row>height = visited
      | Set.member (row,col) visited = visited
      | Set.member (row,col) splitters =
           dfs (row+1,col-1) (dfs (row+1,col+1)
                               (Set.insert (row,col) visited))
      | otherwise =
          dfs (row+1,col) visited

      
-- Part 2
part2 :: Input -> Int
part2 Input{..} = fst (dfs start Map.empty)
  where
    dfs (row,col) memo
      | row>height = (1,memo)
      | Set.member (row,col) splitters =
          case Map.lookup (row,col) memo of
            Just v -> (v, memo)
            Nothing ->
              let (v1,memo') = dfs (row+1,col-1) memo
                  (v2,memo'')= dfs (row+1,col+1) memo'
              in (v1+v2, Map.insert (row,col) (v1+v2) memo'')
      | otherwise =
          dfs (row+1,col) memo

      
