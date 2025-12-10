{-# LANGUAGE RecordWildCards #-}
--
-- Day 10: Factory
--
module Main where

import Data.List
import Data.Bits(xor)
import System.Process (system)

main :: IO ()
main = do
  input <- readInput <$> getContents
  print input
  putStrLn "Part 1"
  print (part1 input)
  putStrLn "Part 2"
  part2 input


data Machine = Machine { target :: [Int]
                       , buttons :: [[Int]]
                       , jolts :: [Int]
                       }
             deriving Show

type Input = [Machine]

readInput :: String -> Input
readInput = map readMachine . lines

readMachine :: String -> Machine
readMachine txt = Machine {..}
  where
    list = words txt
    target = readTarget (head list)
    buttons = map (pad (length jolts)) $ readButtons (init (tail list))
    jolts = readJolts (last list)

pad :: Int -> [Int] -> [Int]
pad n xs = take n (xs ++ repeat 0)
        
readButtons :: [String] -> [[Int]]
readButtons = map readButton

readButton :: String -> [Int]
readButton ('(':txt)
  = toBits $ sum $ map (2^) $ read ("[" ++ init txt ++ "]")

readTarget :: String -> [Int]
readTarget ('[':txt) = map trans $ init txt
  where
    trans '.' = 0
    trans '#' = 1
    trans _ = error "invalid target input"

readJolts :: String -> [Int]
readJolts ('{':txt) = read ('[':init txt ++ "]")

toBits :: Int -> [Int]
toBits 0 = []
toBits n | n>0 = (n`mod`2) :  toBits (n`div`2)

fromBits :: [Int] -> Int
fromBits = foldr (\x acc -> x+2*acc) 0 
                      

-- Part 1
part1 :: Input -> Int
part1 = sum . map solution1

-- minimum number of buttons to start a machine
solution1 :: Machine -> Int
solution1 Machine{..}
  = let numberTarget = fromBits target
        numberButtons = map fromBits buttons
    in 
      case [length bs | bs <- subsequences numberButtons
                      , numberTarget == foldl' xor 0 bs] of
        [] -> error "unsolved machine"
        sols -> minimum sols

-- Part 2 (using GLPK constraint solver)
part2 :: Input -> IO ()
part2 list = do
  system "rm machine*.lp output*.txt"
  sequence_ [runSolver i m | (i, m) <- zip [1..] list]
  system "grep Objective output*.txt | cut -d ' ' -f 5 | awk '{s+=$1}END{print s}'"
  return ()

runSolver :: Int -> Machine -> IO ()
runSolver i m = do
  let path = "machine" ++ show i ++ ".lp"
  let output = "output" ++ show i ++ ".txt"       
  putStrLn ("Generating " ++ path)
  writeFile path (genLP m)
  system ("glpsol --lp " ++ path ++ " -o " ++ output)
  return ()
  
              

-- generate an LP problem for solving Part 2 for a single machine
genLP :: Machine -> String
genLP Machine{..}
  = unlines ( [ "Minimize " ++ obj
              , "Subject To"] ++ cs ++
              ["Bounds"] ++  bounds ++
              ["General", concat (intersperse " " vars), "End"]
            )
  where numVars = length buttons
        vars = ["x" ++ show i | i<-[1..numVars]]
        bounds = ["0<="++var | var <- vars]
        obj = concat (intersperse "+" vars)
        cs = [ equation row | row <- transpose (buttons ++ [jolts])]
        equation row = concat (intersperse " + " $
                        zipWith (\k v -> show k ++ " "++v) row vars)
                       ++ " = "
                       ++ show (last row)
