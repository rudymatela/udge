data Rectangle  =  Rectangle { unit :: String
                             , width :: Int
                             , height :: Int
                             }

area :: Rectangle -> Int
area (Rectangle _ w h)  =  w * h

perimeter :: Rectangle -> Int
perimeter (Rectangle _ w h)  =  2 * (w + h)

main :: IO ()
main  =  interact solve

solve :: String -> String
solve  =  unlines . map (solve1 . readRectangle) . lines

readRectangle :: String -> Rectangle
readRectangle s  =  Rectangle u (read w') (read h')  where  [w',h',u] = words s

solve1 :: Rectangle -> String
solve1 rectangle  =  "The area is " ++ show (area rectangle)
                  ++ " square " ++ unit rectangle
                  ++ " and the perimeter is " ++ show (perimeter rectangle)
                  ++ " " ++ unit rectangle ++ "."
