-module(map).
-export([generate_map/0, object_to_map/1]).

-include("map_records.hrl").

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
        #laser{name = <<"las0">>, p1 = #point{x = 0, y = 9}, p2 = #point{x = 15, y = 9}, enabled = true, interval = 0}
    ], start = #start{point = #point{x = 3, y = 12}, direction = right},
    goal = #point{x = 13, y = 2}}.

object_to_map(#laser{name = Name, p1 = P1, p2 = P2, enabled = Enabled, interval = Interval}) ->
    #{name => Name, p1 => object_to_map(P1), p2 => object_to_map(P2), enabled => Enabled, interval => Interval};

object_to_map(#point{x = X, y = Y}) ->
    #{x => X, y => Y}.
