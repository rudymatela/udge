-- examples/add/add.hs: solution to the "rectangle" example problem
--
-- This program is an example solution to the "rectangle" example problem
-- that gets a full 3/3 score.
--
-- This file is part of Udge.
--
--
-- Copyright (C) 2020-2023  Rudy Matela
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

data Rectangle  =  Rectangle { width :: Int
                             , height :: Int
                             }

area :: Rectangle -> Int
area (Rectangle w h)  =  w * h

perimeter :: Rectangle -> Int
perimeter (Rectangle w h)  =  2 * (w + h)

main :: IO ()
main  =  interact solve

solve :: String -> String
solve  =  unlines . map (solve1 . readRectangle) . lines

readRectangle :: String -> Rectangle
readRectangle s  =  Rectangle w h  where  [w,h] = map read $ words s

solve1 :: Rectangle -> String
solve1 rectangle  =  show (width rectangle) ++ "x" ++ show (height rectangle) ++ " rectangle, "
                  ++ "area = " ++ show (area rectangle) ++ ", "
                  ++ "perimeter = " ++ show (perimeter rectangle)
