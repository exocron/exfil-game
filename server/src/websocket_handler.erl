-module(websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

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
    {[{text, Out}], {game, Pid}};

websocket_handle({text, JSON}, {game, Pid}) ->
    {websocket_handle_json(Pid, jiffy:decode(JSON, [return_maps])), {game, Pid}}.

websocket_handle_json(Game, #{<<"action">> := <<"setname">>, <<"name">> := Name}) ->
    game:client_set_name(Game, Name).

websocket_info(game_is_full, {game, Pid}) ->
    JSON = jiffy:encode(#{action => <<"statechange">>, newstate => <<"role_select">>}),
    {[{text, JSON}], {game, Pid}};

websocket_info({peer_name_changed, Name}, {game, Pid}) ->
    io:format("debug3~n"),
    JSON = jiffy:encode(#{action => <<"rnamechange">>, name => Name}),
    {[{text, JSON}], {game, Pid}}.
