module Sum2 (main) where

import Main (add)

main' :: String -> String
main' = unlines . map (show . (\[x,y] -> add x y) . map read . words) . lines

main :: IO ()
main = interact main'
