%%%-------------------------------------------------------------------
%%% @author exocron
%%% @copyright (C) 2020, exocron
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2020 by exocron
%%%-------------------------------------------------------------------

-module(terminal).
-behaviour(gen_server).

-export([start_link/1, send_input/2, init/1, handle_cast/2]).
-record(state, {client, game}).

start_link(Game) -> gen_server:start_link(terminal, #state{client = self(), game = Game}, []).

send_input(Pid, Input) -> gen_server:cast(Pid, {input, Input}).

init(State) ->
    State#state.client ! {output, <<"QS-DOS 3.13.0\nCopyright (C) 2010-2012 Quantum Softworks, Inc.\n\nC:\\>">>},
    {ok, State}.

handle_cast({input, <<>>}, State) ->
    State#state.client ! {output, <<"C:\\>">>},
    {noreply, State};

handle_cast({input, Text}, State) ->
    Lines = string:lexemes(Text, "\n"),
    [run_command(State#state.client, parse_command(Line)) || Line <- Lines],
    {noreply, State}.

parse_command(Line) ->
    [Cmd | Args] = string:lexemes(Line, " \t\r\v\f"),
    AtomCmd = case normalize_command_name(Cmd) of
        <<"lcontrol">> -> lcontrol;
        _ -> unknown
    end,
    [AtomCmd, Cmd | Args].

normalize_command_name(<<"C:\\">>) -> <<"C:\\">>;
normalize_command_name(<<"C:\\", Rest/binary>>) -> normalize_command_name_2(Rest);
normalize_command_name(<<"\\">>) -> <<"\\">>;
normalize_command_name(<<"\\", Rest/binary>>) -> normalize_command_name_2(Rest);
normalize_command_name(Cmd) -> normalize_command_name_2(Cmd).

normalize_command_name_2(<<".\\">>) -> <<".\\">>;
normalize_command_name_2(<<".\\", Rest/binary>>) -> normalize_command_name_2(Rest);
normalize_command_name_2(<<"..\\">>) -> <<"..\\">>;
normalize_command_name_2(<<"..\\", Rest/binary>>) -> normalize_command_name_2(Rest);
normalize_command_name_2(Cmd) ->
    case binary:longest_common_suffix([Cmd, <<".exe">>]) of
        4 -> binary:part(Cmd, 0, byte_size(Cmd) - 4);
        _ -> Cmd
    end.

run_command(Pid, []) ->
    Pid ! {output, <<"C:\\>">>};

run_command(Pid, [AtomCmd, Cmd | Args]) ->
    case lists:any(fun(X) -> X =:= <<"--help">> orelse X =:= <<"/?">> end, Args) of
        true -> print_help(Pid, AtomCmd, Cmd);
        _ -> run_command(Pid, AtomCmd, Cmd, Args)
    end.

run_command(Pid, lcontrol, _, Args) ->
    Pid ! {output, <<"\nHi\n\nC:\\>">>};

run_command(Pid, _, Cmd, _) ->
    Pid ! {output, <<"'", Cmd/binary, "' is not recognized as an internal or external command,\noperable program or batch file.\n\nC:\\>">>}.

print_help(Pid, lcontrol, _) ->
    Pid ! {output, <<
        "Laser Control Module version 2.14\n"
        "Copyright (C) 2015-2020 Quantum Security, Inc\n"

        "Usage:\n"
        "    lcontrol list - list all managed lasers\n"
        "    lcontrol enable <name> - enable a laser\n"
        "    lcontrol disable <name> - disable a laser\n"
        "    lcontrol pulse <name> - set a pulse interval (in milliseconds)\n"
        "                            0 = no pulse\n"
        "\n"
        "C:\\>"
    >>}.
