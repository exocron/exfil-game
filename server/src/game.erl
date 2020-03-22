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

-export([new/0, delete/1, set_key/2, get_key/1, notify_manager_shutdown/1]).
-export([add_client/2, reconnect_client/2, client_set_name/2, client_select_role/2, client_commit_role/2, client_is_hacker/1]).
-export([set_terminal_pid/2, hacker_list_lasers/1, hacker_configure_laser/4]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

-record(clientinfo, {
    pid,
    ref,
    name = nil,
    role = undecided,
    commit_role = false
}).

-record(gamestate, {
    key = nil,
    client1 = nil,
    client2 = nil,
    state = pregame,
    map = nil
}).

-define(update_hacker_client(State, Params), ((fun() ->
    OldClient = get_hacker_client(State),
    NewClient = OldClient#clientinfo Params,
    set_hacker_client(State, NewClient)
end)())).

-define(update_operative_client(State, Params), ((fun() ->
    OldClient = get_operative_client(State),
    NewClient = OldClient#clientinfo Params,
    set_operative_client(State, NewClient)
end)())).

-include("map_records.hrl").

new() -> gen_server:start(game, [], []).
delete(Pid) -> gen_server:stop(Pid).

set_key(Pid, Key) -> gen_server:call(Pid, {set_key, Key}).
get_key(Pid) -> gen_server:call(Pid, get_key).
notify_manager_shutdown(Pid) -> gen_server:cast(Pid, notify_manager_shutdown).

add_client(Pid, Client) -> gen_server:call(Pid, {add_client, Client}).
reconnect_client(Pid, Ref) -> gen_server:call(Pid, {reconnect_client, Ref}).
client_set_name(Pid, Name) -> gen_server:call(Pid, {client_set_name, Name}).
client_select_role(Pid, Role) -> gen_server:call(Pid, {client_select_role, Role}).
client_commit_role(Pid, Bool) -> gen_server:call(Pid, {client_commit_role, Bool}).
client_is_hacker(Pid) -> gen_server:call(Pid, client_is_hacker).

set_terminal_pid(Pid, Terminal) -> gen_server:call(Pid, {set_terminal_pid, Terminal}).
hacker_list_lasers(Pid) -> gen_server:call(Pid, hacker_list_lasers).
hacker_configure_laser(Pid, Name, Enabled, Interval) -> gen_server:call(Pid, {hacker_configure_laser, Name, Enabled, Interval}).

operative_move_location({0, 0}) -> self(), ok.

%% Server

init(_) -> {ok, #gamestate{}}.

handle_cast(notify_manager_shutdown, State) ->
    erlang:send_after(5000, self(), connect_manager),
    {noreply, State}.

handle_call({set_key, Key}, _, State) ->
    {reply, ok, State#gamestate{key = Key}};

handle_call(get_key, _, State) ->
    {reply, {ok, State#gamestate.key}, State};

handle_call({add_client, Pid}, _, State) ->
    handle_duplicate_clients(State, Pid);

handle_call({reconnect_client, Ref}, {Pid, _}, State) ->
    #gamestate{client1 = C1, client2 = C2} = State,
    maybe_reconnect_client_1(State, Pid, Ref, C1, C2);

%% Require that all following calls match one of the clients

handle_call(Request, {Pid, _}, State) when Pid == State#gamestate.client1#clientinfo.pid ->
    handle_call_client(client1, Request, State);

handle_call(Request, {Pid, _}, State) when Pid == State#gamestate.client2#clientinfo.pid ->
    handle_call_client(client2, Request, State);

handle_call(_, _, State) ->
    {reply, {error, not_in_game}, State}.

handle_call_client(client1, {client_set_name, Name}, State) ->
    NewClient = State#gamestate.client1#clientinfo{name = Name},
    case State#gamestate.client2 of
        nil -> nil;
        #clientinfo{pid = Pid} -> Pid ! {peer_name_changed, Name}
    end,
    {reply, ok, State#gamestate{client1 = NewClient}};

handle_call_client(client2, {client_set_name, Name}, State) ->
    NewClient = State#gamestate.client2#clientinfo{name = Name},
    case State#gamestate.client1 of
        nil -> nil;
        #clientinfo{pid = Pid} -> Pid ! {peer_name_changed, Name}
    end,
    {reply, ok, State#gamestate{client2 = NewClient}};

handle_call_client(Client, {client_select_role, Role}, State) when State#gamestate.state =:= pregame ->
    maybe_set_role(Client, Role, State);

handle_call_client(_, {client_select_role, _}, State) ->
    {reply, {error, game_started}, State};

handle_call_client(Client, {client_commit_role, Bool}, State) when State#gamestate.state =:= pregame ->
    maybe_commit_role(Client, Bool, State);

handle_call_client(_, {client_commit_role, _}, State) ->
    {reply, {error, game_started}, State};

handle_call_client(_, client_is_hacker, State) when State#gamestate.state =/= started ->
    {reply, false, State};

handle_call_client(client1, Request, State) when State#gamestate.state =:= started ->
    handle_call_roll(State#gamestate.client1#clientinfo.role, client1, Request, State);

handle_call_client(client2, Request, State) when State#gamestate.state =:= started ->
    handle_call_roll(State#gamestate.client2#clientinfo.role, client2, Request, State).

handle_call_roll(hacker, _, client_is_hacker, State) ->
    {reply, true, State};

handle_call_roll(_, _, client_is_hacker, State) ->
    {reply, false, State};

handle_call_roll(hacker, _, {set_terminal_pid, Terminal}, State) ->
    {reply, ok, ?update_hacker_client(State, {pid = Terminal})};

handle_call_roll(_, _, {set_terminal_pid, _}, State) ->
    {reply, {error, not_hacker}, State};

handle_call_roll(hacker, _, hacker_list_lasers, State) ->
    {reply, {ok, State#gamestate.map#map.lasers}, State};

handle_call_roll(_, _, hacker_list_lasers, State) ->
    {reply, {error, not_hacker}, State};

handle_call_roll(hacker, _, {hacker_configure_laser, Name, Enabled, Interval}, State) ->
    NewLasers = [(case Laser of
        #laser{name = Name} ->
            NewLaser = Laser#laser{enabled = Enabled, interval = Interval},
            notify_operative(State, {game_object_changed, NewLaser}),
            NewLaser;
        _ -> Laser
    end) || Laser <- State#gamestate.map#map.lasers],
    NewMap = State#gamestate.map#map{lasers = NewLasers},
    {reply, ok, State#gamestate{map = NewMap}};

handle_call_roll(_, _, {hacker_configure_laser, _, _, _}, State) ->
    {reply, {error, not_hacker}, State}.

handle_info(connect_manager, State) ->
    case game_manager:add_game(self(), State#gamestate.key) of
        {ok, _, _, _} -> {noreply, State};
        _ ->
            % TODO: notify clients that game is ending
            {stop, insert_game, State}
    end;

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

maybe_add_client(State, Pid, nil, C2) ->
    Ref = make_ref(),
    Client = #clientinfo{pid = Pid, ref = Ref},
    Pid ! {peer_name_changed, C2#clientinfo.name},
    Pid ! game_is_full,
    C2#clientinfo.pid ! game_is_full,
    {reply, {ok, Ref}, State#gamestate{client1 = Client}};

maybe_add_client(State, Pid, C1, nil) ->
    Ref = make_ref(),
    Client = #clientinfo{pid = Pid, ref = Ref},
    Pid ! {peer_name_changed, C1#clientinfo.name},
    Pid ! game_is_full,
    C1#clientinfo.pid ! game_is_full,
    {reply, {ok, Ref}, State#gamestate{client2 = Client}};

maybe_add_client(State, _, _, _) ->
    {reply, {error, game_is_full}, State}.

%% Client Reconnection

maybe_reconnect_client_1(State, Pid, Ref, nil, C2) ->
    maybe_reconnect_client_2(State, Pid, Ref, C2);

maybe_reconnect_client_1(State, Pid, Ref, C1, _) when C1#clientinfo.ref == Ref ->
    NewClient = C1#clientinfo{pid = Pid},
    case State#gamestate.state of
        started -> Pid ! game_is_starting;
        _ -> ok
    end,
    {reply, ok, State#gamestate{client1 = NewClient}};

maybe_reconnect_client_1(State, Pid, Ref, _, C2) ->
    maybe_reconnect_client_2(State, Pid, Ref, C2).

maybe_reconnect_client_2(State, _, _, nil) ->
    {reply, error, State};

maybe_reconnect_client_2(State, Pid, Ref, C2) when C2#clientinfo.ref == Ref ->
    NewClient = C2#clientinfo{pid = Pid},
    case State#gamestate.state of
        started -> Pid ! game_is_starting;
        _ -> ok
    end,
    {reply, ok, State#gamestate{client2 = NewClient}};

maybe_reconnect_client_2(State, _, _, _) ->
    {reply, error, State}.

%% Set client roles

maybe_set_role(client1, Role, State) when Role =:= hacker orelse Role =:= operative orelse Role =:= undecided ->
    State#gamestate.client2#clientinfo.pid ! {peer_role_changed, Role},
    NewClient = State#gamestate.client1#clientinfo{role = Role, commit_role = false},
    {reply, ok, State#gamestate{client1 = NewClient}};

maybe_set_role(client2, Role, State) when Role =:= hacker orelse Role =:= operative orelse Role =:= undecided ->
    State#gamestate.client1#clientinfo.pid ! {peer_role_changed, Role},
    NewClient = State#gamestate.client2#clientinfo{role = Role, commit_role = false},
    {reply, ok, State#gamestate{client2 = NewClient}};

maybe_set_role(_, _, State) ->
    {reply, {error, invalid_role}, State}.

maybe_commit_role(client1, Bool, State) when State#gamestate.client1#clientinfo.role =/= undecided ->
    State#gamestate.client2#clientinfo.pid ! {peer_commit_role_changed, Bool},
    NewClient = State#gamestate.client1#clientinfo{commit_role = Bool},
    if Bool -> self() ! maybe_start_game; true -> true end,
    {reply, ok, State#gamestate{client1 = NewClient}};

maybe_commit_role(client2, Bool, State) when State#gamestate.client2#clientinfo.role =/= undecided ->
    State#gamestate.client1#clientinfo.pid ! {peer_commit_role_changed, Bool},
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
    State#gamestate{state = started, map = map:generate_map()};

maybe_start_game(State) ->
    State.

get_hacker_client(State) when State#gamestate.client1#clientinfo.role =:= hacker ->
    State#gamestate.client1;

get_hacker_client(State) when State#gamestate.client2#clientinfo.role =:= hacker ->
    State#gamestate.client2.

get_operative_client(State) when State#gamestate.client1#clientinfo.role =:= operative ->
    State#gamestate.client1;

get_operative_client(State) when State#gamestate.client2#clientinfo.role =:= operative ->
    State#gamestate.client2.

set_hacker_client(State, Client) when State#gamestate.client1#clientinfo.role =:= hacker ->
    State#gamestate{client1 = Client};

set_hacker_client(State, Client) when State#gamestate.client2#clientinfo.role =:= hacker ->
    State#gamestate{client2 = Client}.

set_operative_client(State, Client) when State#gamestate.client1#clientinfo.role =:= operative ->
    State#gamestate{client1 = Client};

set_operative_client(State, Client) when State#gamestate.client2#clientinfo.role =:= operative ->
    State#gamestate{client2 = Client}.

notify_operative(State, Msg) when State#gamestate.client1#clientinfo.role =:= operative ->
    State#gamestate.client1#clientinfo.pid ! Msg;

notify_operative(State, Msg) when State#gamestate.client2#clientinfo.role =:= operative ->
    State#gamestate.client2#clientinfo.pid ! Msg.
