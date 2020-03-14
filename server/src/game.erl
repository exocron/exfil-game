%%%-------------------------------------------------------------------
%%% @author exocron
%%% @copyright (C) 2020, exocron
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2020 by exocron
%%%-------------------------------------------------------------------

-module(game).
-behaviour(gen_server).

-export([new/0, delete/2, add_client/1, reconnect_client/2,
    client_select_role/2, client_commit_role/2]).

-export([init/1, handle_call/3, handle_info/2]).

-record(clientinfo, {
    pid,
    ref,
    role = undecided,
    commit_role = false
}).

-record(gamestate, {
    client1 = nil,
    client2 = nil,
    state = pregame
}).

new() -> gen_server:start(game, [], []).
delete(Pid, Reason) -> gen_server:stop(Pid, Reason).

add_client(Pid) -> gen_server:call(Pid, add_client).
reconnect_client(Pid, Ref) -> gen_server:call(Pid, {reconnect_client, Ref}).
client_select_role(Pid, Role) -> gen_server:call(Pid, {client_select_role, Role}).
client_commit_role(Pid, Bool) -> gen_server:call(Pid, {client_commit_role, Bool}).

operative_move_location({0, 0}) -> self(), ok.

hacker_configure_laser(<<"las0">>, false, 0) -> self(), ok.

%% Server

init(_) -> {ok, #gamestate{}}.

handle_call(add_client, {Pid, _}, State) ->
    handle_duplicate_clients(State, Pid);

handle_call({reconnect_client, Ref}, {Pid, _}, State) ->
    #gamestate{client1 = C1, client2 = C2} = State,
    maybe_reconnect_client_1(State, Pid, Ref, C1, C2);

%% Require that all following calls match one of the clients

handle_call(Request, {Pid, _} = From, State) when Pid == State#gamestate.client1#clientinfo.pid ->
    handle_call_client(client1, Request, State);

handle_call(Request, {Pid, _} = From, State) when Pid == State#gamestate.client2#clientinfo.pid ->
    handle_call_client(client2, Request, State);

handle_call(_, _, State) ->
    {reply, {error, not_in_game}, State}.

handle_call_client(Client, {client_select_role, Role}, State) when State#gamestate.state =:= pregame ->
    maybe_set_role(Client, Role, State);

handle_call_client(_, {client_select_role, _}, State) ->
    {reply, {error, game_started}, State};

handle_call_client(Client, {client_commit_role, Bool}, State) when State#gamestate.state =:= pregame ->
    maybe_commit_role(Client, Bool, State);

handle_call_client(Client, {client_commit_role, _}, State) ->
    {reply, {error, game_started}, State}.

handle_info(maybe_start_game, State) ->
    {noreply, maybe_start_game(State)}.

%% Boring client management routines below (duplicate and game full)

handle_duplicate_clients(State, Pid) ->
    #gamestate{client1 = C1, client2 = C2} = State,
    handle_duplicate_client_1(State, Pid, C1, C2).

handle_duplicate_client_1(State, Pid, nil, C2) ->
    handle_duplicate_client_2(State, Pid, nil, C2);

handle_duplicate_client_1(State, Pid, C1, _) when C1#clientinfo.pid == Pid ->
    {reply, {ok, C1#clientinfo.ref}, State};

handle_duplicate_client_1(State, Pid, C1, C2) ->
    handle_duplicate_client_2(State, Pid, C1, C2).

handle_duplicate_client_2(State, Pid, C1, nil) ->
    maybe_add_client(State, Pid, C1, nil);

handle_duplicate_client_2(State, Pid, _, C2) when C2#clientinfo.pid == Pid ->
    {reply, {ok, C2#clientinfo.ref}, State};

handle_duplicate_client_2(State, Pid, C1, C2) ->
    maybe_add_client(State, Pid, C1, C2).

maybe_add_client(State, Pid, nil, nil) ->
    Ref = make_ref(),
    Client = #clientinfo{pid = Pid, ref = Ref},
    {reply, {ok, Ref}, State#gamestate{client1 = Client}};

maybe_add_client(State, Pid, nil, _) ->
    Ref = make_ref(),
    Client = #clientinfo{pid = Pid, ref = Ref},
    {reply, {ok, Ref}, State#gamestate{client1 = Client}};

maybe_add_client(State, Pid, _, nil) ->
    Ref = make_ref(),
    Client = #clientinfo{pid = Pid, ref = Ref},
    {reply, {ok, Ref}, State#gamestate{client2 = Client}};

maybe_add_client(State, _, _, _) ->
    {reply, {error, game_is_full}, State}.

%% Client Reconnection

maybe_reconnect_client_1(State, Pid, Ref, nil, C2) ->
    maybe_reconnect_client_2(State, Pid, Ref, C2);

maybe_reconnect_client_1(State, Pid, Ref, C1, _) when C1#clientinfo.ref == Ref ->
    NewClient = C1#clientinfo{pid = Pid},
    {reply, ok, State#gamestate{client1 = NewClient}};

maybe_reconnect_client_1(State, Pid, Ref, C1, C2) ->
    maybe_reconnect_client_2(State, Pid, Ref, C2).

maybe_reconnect_client_2(State, Pid, Ref, nil) ->
    {reply, error, State};

maybe_reconnect_client_2(State, Pid, Ref, C2) when C2#clientinfo.ref == Ref ->
    NewClient = C2#clientinfo{pid = Pid},
    {reply, ok, State#gamestate{client2 = NewClient}};

maybe_reconnect_client_2(State, Pid, Ref, _) ->
    {reply, error, State}.

%% Set client roles

maybe_set_role(client1, Role, State) when Role =:= hacker orelse Role =:= operative orelse Role =:= undecided ->
    NewClient = State#gamestate.client1#clientinfo{role = Role, commit_role = false},
    {reply, ok, State#gamestate{client1 = NewClient}};

maybe_set_role(client2, Role, State) when Role =:= hacker orelse Role =:= operative orelse Role =:= undecided ->
    NewClient = State#gamestate.client2#clientinfo{role = Role, commit_role = false},
    {reply, ok, State#gamestate{client2 = NewClient}};

maybe_set_role(_, _, State) ->
    {reply, {error, invalid_role}, State}.

maybe_commit_role(client1, Bool, State) when State#gamestate.client1#clientinfo.role =/= undecided ->
    NewClient = State#gamestate.client1#clientinfo{commit_role = Bool},
    if Bool -> self() ! maybe_start_game; true -> true end,
    {reply, ok, State#gamestate{client1 = NewClient}};

maybe_commit_role(client2, Bool, State) when State#gamestate.client2#clientinfo.role =/= undecided ->
    NewClient = State#gamestate.client2#clientinfo{commit_role = Bool},
    if Bool -> self() ! maybe_start_game; true -> true end,
    {reply, ok, State#gamestate{client2 = NewClient}};

maybe_commit_role(_, _, State) ->
    {reply, {error, invalid_role}, State}.

maybe_start_game(State) when State#gamestate.client1 == nil orelse State#gamestate.client2 == nil ->
    State;

maybe_start_game(State) when State#gamestate.client1#clientinfo.commit_role =:= true andalso State#gamestate.client2#clientinfo.commit_role =:= true ->
    State#gamestate.client1#clientinfo.pid ! game_is_starting,
    State#gamestate.client2#clientinfo.pid ! game_is_starting,
    State#gamestate{state = started}.