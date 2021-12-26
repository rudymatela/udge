% examples/add/add.erl: example correct solution to "add"
%
% This is an incorrect solution to the "add" problem.
% It should get a full 1/1 score.
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
-module(add).
-export([main/1,add/2]).

add(X,Y) ->
    X + Y.

main(_) ->
    loop().

loop() ->
    case io:fread("", "~d ~d") of
        eof ->
            ok;
        {ok, [X,Y]} ->
            io:format("~p~n", [add(X,Y)]),
            loop()
    end.
