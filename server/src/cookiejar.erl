%%%-------------------------------------------------------------------
%%% @author exocron
%%% @copyright (C) 2020, exocron
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2020 by exocron
%%%-------------------------------------------------------------------

-module(cookiejar).
-behaviour(gen_server).

-export([start_link/0, get_cookie/1, get_ref/1, init/1, handle_call/3]).

-ifdef(rad).
-export([set_dbg_hacker_ref/1, set_dbg_operative_ref/1]).
-endif.

start_link() -> gen_server:start_link({local, cookiejar}, cookiejar, [], []).

get_cookie(Ref) -> gen_server:call(cookiejar, {get_cookie, Ref}).
get_ref(Cookie) -> gen_server:call(cookiejar, {get_ref, Cookie}).

-ifdef(rad).
set_dbg_hacker_ref(Ref) -> gen_server:call(cookiejar, {set_dbg_hacker_ref, Ref}).
set_dbg_operative_ref(Ref) -> gen_server:call(cookiejar, {set_dbg_operative_ref, Ref}).
-endif.

%% Server

init(_) ->
    ets:new(cookie_ref, [named_table]),
    ets:new(ref_cookie, [named_table]),
    {ok, []}.

handle_call({get_cookie, Ref}, _, []) ->
    case ets:lookup(ref_cookie, Ref) of
        [{Ref, Cookie}] -> {reply, {ok, Cookie}, []};
        _ ->
            Cookie = string:trim(base64:encode(crypto:strong_rand_bytes(16)), trailing, "="),
            handle_new_cookie(Ref, Cookie, ets:insert(ref_cookie, {Ref, Cookie}), ets:insert(cookie_ref, {Cookie, Ref}))
    end;

handle_call({get_ref, Cookie}, _, []) ->
    case ets:lookup(cookie_ref, Cookie) of
        [{Cookie, Ref}] -> {reply, {ok, Ref}, []};
        _ -> {reply, {error, invalid_cookie}, []}
    end;

handle_call({set_dbg_hacker_ref, Ref}, _, []) ->
    true = ets:insert(ref_cookie, {Ref, <<"h">>}),
    true = ets:insert(cookie_ref, {<<"h">>, Ref}),
    {reply, ok, []};

handle_call({set_dbg_operative_ref, Ref}, _, []) ->
    true = ets:insert(ref_cookie, {Ref, <<"o">>}),
    true = ets:insert(cookie_ref, {<<"o">>, Ref}),
    {reply, ok, []}.

handle_new_cookie(_, _, false, false) ->
    {reply, {error, insert_cookie}, []};

handle_new_cookie(Ref, _, true, false) ->
    ets:delete(ref_cookie, Ref),
    {reply, {error, insert_cookie}, []};

handle_new_cookie(_, Cookie, false, true) ->
    ets:delete(cookie_ref, Cookie),
    {reply, {error, insert_cookie}, []};

handle_new_cookie(_, Cookie, true, true) ->
    {reply, {ok, Cookie}, []}.
