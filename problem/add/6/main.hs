module AltMain (main) where

import Main (add)

main' :: String -> String
main' = unlines . map (show . (\[x,y] -> add x y) . map read . words) . lines

main :: IO ()
main = do
  -- processes standard input
  interact main'

  -- processes in.txt
  more <- readFile "in.txt"
  putStr $ main' more
