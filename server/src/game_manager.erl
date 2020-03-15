%%%-------------------------------------------------------------------
%%% @author exocron
%%% @copyright (C) 2020, exocron
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2020 by exocron
%%%-------------------------------------------------------------------

-module(game_manager).
-behaviour(gen_server).

-export([start_link/0, add_game/0, add_game/2, find_game/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

start_link() -> gen_server:start_link({local, game_manager}, game_manager, [], []).

add_game() -> gen_server:call(game_manager, add_game).
add_game(Game, Key) -> gen_server:call(game_manager, {add_game, Game, Key}).
find_game(Key) -> gen_server:call(game_manager, {find_game, Key}).

%% Server

init(_) ->
    ets:new(game_tbl, [named_table]),
    ets:new(game_rev, [named_table]),
    {ok, []}.

handle_call(add_game, From, []) ->
    maybe_add_game(From, game:new());

handle_call({add_game, Game, Key}, _, []) ->
    maybe_insert_game(Key, Game, [], ets:insert_new(game_tbl, {Key, Game}), ets:insert_new(game_rev, {Game, Key})).

handle_info({'DOWN', _, process, Pid, _}, []) ->
    case ets:lookup(game_rev, Pid) of
        [{Pid, Key}] ->
            ets:delete(game_rev, Pid),
            ets:delete(game_tbl, Key);
        _ -> []
    end,
    {noreply, []}.

terminate(_, _) ->
    ets:foldl(fun({Pid, _}, _) -> game:notify_manager_shutdown(Pid) end, [], game_rev).

%% Implementation

maybe_add_game({From, _}, {ok, Pid}) ->
    maybe_add_client(Pid, game:add_client(Pid, From));

maybe_add_game(_, Result) ->
    {reply, Result, []}.

maybe_add_client(Pid, {ok, Ref}) ->
    Key = keygen:generate_key(),
    ok = game:set_key(Pid, Key),
    maybe_insert_game(Key, Pid, Ref, ets:insert_new(game_tbl, {Key, Pid}), ets:insert_new(game_rev, {Pid, Key}));

maybe_add_client(Pid, Result) ->
    game:delete(Pid),
    {reply, Result, []}.

maybe_insert_game(_, Pid, _, false, false) ->
    game:delete(Pid),
    {reply, {error, insert_new}, []};

maybe_insert_game(_, Pid, _, false, true) ->
    ets:delete(game_rev, Pid),
    game:delete(Pid),
    {reply, {error, insert_new}, []};

maybe_insert_game(Key, Pid, _, true, false) ->
    ets:delete(game_tbl, Key),
    game:delete(Pid),
    {reply, {error, insert_new}, []};

maybe_insert_game(Key, Pid, Ref, true, true) ->
    erlang:monitor(process, Pid),
    {reply, {ok, Key, Pid, Ref}, []}.
