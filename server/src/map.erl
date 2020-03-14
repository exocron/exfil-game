-module(map).
-export([generate_map/0]).

-record(laser, {x, y, direction}).
-record(start, {x, y, direction}).
-record(goal, {x, y}).
-record(map, {tiles, lasers, start, goal}).

generate_map() ->
    % Random maps? In my game jam?!
    #map{tiles = [
        <<"WWWWWWWWWWWWWWWW">>,
        <<"W..............W">>,
        <<"W............G.W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"W.S............W">>,
        <<"W..............W">>,
        <<"W..............W">>,
        <<"WWWWWWWWWWWWWWWW">>
    ], lasers = [
        #laser{x = 0, y = 9, direction = right}
    ], start = #start{x = 3, y = 12, direction = right},
    goal = #goal{x = 13, y = 2}}.
