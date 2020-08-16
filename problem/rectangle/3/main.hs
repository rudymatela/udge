-- main.hs: main file for an "rectangle" solution
--
-- This imports the add function from submission file as a Main module
--
-- It processes standard input then the "in.txt" file.
--
--
-- Copyright (C) 2020  Rudy Matela
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
module AltMain (main) where

import Main (Rectangle(..), area, perimeter)

main :: IO ()
main = do
  -- processes standard input
  interact solve

  -- processes in.txt
  more <- readFile "in.txt"
  putStr $ solve more

solve :: String -> String
solve  =  unlines . map (solve1 . readRectangle) . lines

readRectangle :: String -> Rectangle
readRectangle s  =  Rectangle w h  where  [w,h] = map read $ words s

solve1 :: Rectangle -> String
solve1 rectangle  =  show (width rectangle) ++ "x" ++ show (height rectangle) ++ " rectangle, "
                  ++ "area = " ++ show (area rectangle) ++ ", "
                  ++ "perimeter = " ++ show (perimeter rectangle)
