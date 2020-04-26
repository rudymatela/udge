module Main (main, add) where

add :: Int -> Int -> Int
add x y = x + y

main' :: String -> String
main' = unlines . map (show . (\[x,y] -> add x y) . map read . words) . lines

main :: IO ()
main = interact main'
