main' :: String -> String
main' = unlines . map (show . (\[x,y] -> x+y) . map read . words) . lines

main :: IO ()
main = interact main'
