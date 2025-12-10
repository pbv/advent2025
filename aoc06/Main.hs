--
-- Day 6: Trash Compactor
--
module Main where
import Data.List

main :: IO ()
main = do
  txt <- getContents
  putStrLn "Part 1"
  print (part1 txt)
  putStrLn "Part 2"
  print (part2 txt)


-- Part 1
part1 :: String -> Int
part1 = sum . map computeRow . transpose . map words . lines

computeRow :: [String] -> Int
computeRow row
  = case last row of
      "+" -> sum items
      "*" -> product items
      _ -> error "invalid operation"
  where items = map read (init row)

-- Part 2
part2 :: String -> Int
part2 = sum . map computeRows . split . transpose . map reverse . lines

-- split problems
split :: [String] -> [[String]]
split [] = []
split (x:xs) | isBlank x = split xs
split xs = takeWhile (not.isBlank) xs : split (dropWhile (not.isBlank) xs)

-- compute a problem; the parsing is a bit of hack
computeRows :: [String] -> Int
computeRows items 
  = case last (last items) of
        '+' -> sum values
        '*' -> product values
        _ -> error "invalid operator"
  where values = map read (init items ++ [init (last items)])

isBlank :: String -> Bool
isBlank = all (==' ')
