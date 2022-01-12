% main.erl: main file for an "add" solution
%
% This file is part of Udge.
%
%
% Copyright (C) 2022  Rudy Matela
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
-module(main).
-export([main/1]).
-import(thelib,[add/2]).

main(_) ->
    loop(standard_io),
    {ok, File} = file:open("in.txt",read),
    loop(File),
    file:close(File).

loop(F) ->
    case io:fread(F, "", "~d ~d") of
        eof ->
            ok;
        {ok, [X,Y]} ->
            io:format("~p~n", [add(X,Y)]),
            loop(F)
    end.
