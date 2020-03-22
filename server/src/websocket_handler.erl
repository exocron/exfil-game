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
    self() ! maybe_create_terminal,
    {[{text, Out}], {game, Pid}};

websocket_handle({text, JSON}, {game, Pid}) ->
    {websocket_handle_json(Pid, jiffy:decode(JSON, [return_maps])), {game, Pid}};

websocket_handle({binary, Data}, {terminal, Pid}) ->
    terminal:send_input(Pid, Data),
    {ok, {terminal, Pid}}.

websocket_handle_json(Game, #{<<"action">> := <<"setname">>, <<"name">> := Name}) ->
    game:client_set_name(Game, Name);

websocket_handle_json(Game, #{<<"action">> := <<"setrole">>, <<"role">> := <<"undecided">>}) ->
    game:client_select_role(Game, undecided);

websocket_handle_json(Game, #{<<"action">> := <<"setrole">>, <<"role">> := <<"operative">>}) ->
    game:client_select_role(Game, operative);

websocket_handle_json(Game, #{<<"action">> := <<"setrole">>, <<"role">> := <<"hacker">>}) ->
    game:client_select_role(Game, hacker);

websocket_handle_json(Game, #{<<"action">> := <<"ready">>, <<"ready">> := Ready}) ->
    game:client_commit_role(Game, Ready).

websocket_info(game_is_full, {game, Pid}) ->
    JSON = jiffy:encode(#{action => <<"statechange">>, newstate => <<"role_select">>}),
    {[{text, JSON}], {game, Pid}};

websocket_info({peer_name_changed, Name}, {game, Pid}) ->
    JSON = jiffy:encode(#{action => <<"rnamechange">>, name => Name}),
    {[{text, JSON}], {game, Pid}};

websocket_info({peer_role_changed, Role}, {game, Pid}) ->
    JSON = jiffy:encode(#{action => <<"rrolechange">>, role => Role}),
    {[{text, JSON}], {game, Pid}};

websocket_info({peer_commit_role_changed, Ready}, {game, Pid}) ->
    JSON = jiffy:encode(#{action => <<"rready">>, ready => Ready}),
    {[{text, JSON}], {game, Pid}};

websocket_info(game_is_starting, {game, Pid}) ->
    JSON = jiffy:encode(#{action => <<"statechange">>, newstate => <<"game_start">>}),
    {[{text, JSON}], {game, Pid}};

websocket_info({game_object_changed, Object}, {game, Pid}) ->
    Type = element(1, Object),
    MapObject = (map:object_to_map(Object))#{type => Type},
    JSON = jiffy:encode(#{action => <<"objectchange">>, object => MapObject}),
    {[{text, JSON}], {game, Pid}};

websocket_info(maybe_create_terminal, {game, Pid}) ->
    case game:client_is_hacker(Pid) of
        true ->
            {ok, TermPid} = terminal:start_link(Pid),
            game:set_terminal_pid(Pid, TermPid),
            {ok, {terminal, TermPid}};
        _ ->
            {ok, {game, Pid}}
    end;

websocket_info({output, Text}, {terminal, Pid}) ->
    {[{binary, Text}], {terminal, Pid}}.
