--
-- Day 11: Reactor
--
module Main where

import           Data.List
import           Data.Maybe

import           Data.Map (Map)
import qualified Data.Map.Strict as Map

import           Control.Monad
import           Control.Monad.State.Strict

main :: IO ()
main = do
  inp <- readInput <$> getContents
  putStrLn "Part 1:"
  print (part1 inp)
  putStrLn "Part 2:"
  print (part2' inp)

type Node = String
type Input = Map Node [Node]

readInput :: String -> Input
readInput = Map.fromList . map readConnections . lines

readConnections :: String -> (Node,[Node])
readConnections txt = (init first, rest)
  where first:rest = words txt



-- Part 1
-- memo for countings paths starting from a node
type Memo = Map Node Int

lookupMemo :: Node -> State Memo (Maybe Int)
lookupMemo node = do
  memo <- get
  return (Map.lookup node memo)

writeMemo :: Node -> Int -> State Memo ()
writeMemo node dist = do
  memo <- get
  put (Map.insert node dist memo)
  

part1 :: Input -> Int
part1 graph = evalState (dfs graph "you" "out") Map.empty

dfs :: Input -> Node -> Node -> State Memo Int
dfs graph node end 
  | node == end =
      return 1
  | otherwise = do
      optd <- lookupMemo node
      case optd of
        Just d -> return d
        Nothing -> do
          dists <- sequence [ dfs graph node' end
                            | node' <- fromMaybe []
                                       (Map.lookup node graph)
                            ]
          let dist = sum dists
          writeMemo node dist
          return dist


-- Part 2
-- number of paths from "svr" to "out"
-- that pass through extra nodes "dac" and "fft"
-- Idea: count all paths and subtract the ones that
-- don't pass throught dac, fft; add the ones in the intersection
-- because they were subtracted twice

part2 :: Input -> Int
part2 graph = all - noDac - noFft + noDacFft
  where
    graph1 = Map.delete "dac" graph
    graph2 = Map.delete "fft" graph
    graph3 = Map.delete "dac" graph2
    all = evalState (dfs graph "svr" "out") Map.empty
    noDac = evalState (dfs graph1 "svr" "out") Map.empty
    noFft = evalState (dfs graph2 "svr" "out") Map.empty
    noDacFft =evalState (dfs graph3 "svr" "out") Map.empty
      

-- Alternative
-- factor paths as svr -> dac -> fft -> out
-- and svr -> fft -> dac -> out
part2' :: Input -> Int
part2' graph = paths1*paths2*paths3 + paths4*paths5*paths6
  where
    paths1 = evalState (dfs graph "svr" "dac") Map.empty
    paths2 = evalState (dfs graph "dac" "fft") Map.empty
    paths3 = evalState (dfs graph "fft" "out") Map.empty
    paths4 = evalState (dfs graph "svr" "fft") Map.empty
    paths5 = evalState (dfs graph "fft" "dac") Map.empty
    paths6 = evalState (dfs graph "dac" "out") Map.empty
    
