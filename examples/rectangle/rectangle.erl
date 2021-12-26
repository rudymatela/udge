% examples/rectangle/rectangle.erl: example correct solution to "rectangle"
%
% This is an incorrect solution to the "rectangle" problem.
% It should get a full 3/3 score.
%
% This file is part of Udge.
%
%
% Copyright (C) 2020-2021  Rudy Matela
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
-export([main/1,area/1,perimeter/1]).

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
