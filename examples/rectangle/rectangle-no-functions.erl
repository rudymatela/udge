% examples/rectangle/rectangle-no-functions.erl: example incorrect solution to "rectangle"
%
% This is an incorrect solution to the "rectangle" problem.
% It should get a 2/3 score due to missing functions.
%
% This file is part of Udge.
%
%
% Copyright (C) 2020-2022  Rudy Matela
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
-module(rectangel).
-export([main/1]). % simply not export the functions

-record(rectangle, {width, height}).

area(R) ->
    R#rectangle.width * R#rectangle.height.

perimeter(R) ->
    2 * (R#rectangle.width + R#rectangle.height).

main(_) ->
    loop().

loop() ->
    case io:fread("", "~d ~d") of
        eof ->
            ok;
        {ok, [W,H]} ->
            R = #rectangle{width=W, height=H},
            io:format("~px~p rectangle, area = ~p, perimeter = ~p~n", [W,H,area(R),perimeter(R)]),
            loop()
    end.
