-module(websocket_handler).
-export([init/2, websocket_init/1]).

init(Req, new) ->
    io:format("New: ~p~n", [Req]),
    {cowboy_websocket, Req, new};

init(Req, join) ->
    io:format("Join: ~p~n", [Req]),
    {cowboy_websocket, Req, join}.

websocket_init(new) ->
    io:format("WebSocket Init New~n"),
    {ok, Key, Pid, Ref} = game_manager:add_game(),
    {ok, Cookie} = cookiejar:get_cookie(Ref),
    JSON = jiffy:encode(#{code => Key, token => Cookie}),
    {[{text, JSON}], {game, Pid}};

websocket_init(join) ->
    io:format("WebSocket Init Join~n"),
    {ok, join}.
