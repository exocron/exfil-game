-module(websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2]).

init(Req, new) ->
    {cowboy_websocket, Req, new};

init(Req, join) ->
    {cowboy_websocket, Req, join}.

websocket_init(new) ->
    {ok, Key, Pid, Ref} = game_manager:add_game(),
    {ok, Cookie} = cookiejar:get_cookie(Ref),
    JSON = jiffy:encode(#{code => Key, token => Cookie}),
    {[{text, JSON}], {game, Pid}};

websocket_init(join) ->
    io:format("WebSocket Init Join~n"),
    {ok, wait_for_game_info}.

websocket_handle({text, JSON}, wait_for_game_info) ->
    {Pid, Out} = case jiffy:decode(JSON, [return_maps]) of
        #{<<"code">> := Code, <<"token">> := Token} ->
            {ok, Pid} = game_manager:find_game(Code),
            {ok, Ref} = cookiejar:get_ref(Token),
            ok = game:reconnect_client(Pid, Ref),
            {Pid, jiffy:encode(#{code => Code, token => Token})};
        #{<<"code">> := Code} ->
            {ok, Pid} = game_manager:find_game(Code),
            {ok, Ref} = game:add_client(Pid, self()),
            {ok, Cookie} = cookiejar:get_cookie(Ref),
            {Pid, jiffy:encode(#{code => Code, token => Cookie})}
    end,
    {[{text, Out}], {game, Pid}}.