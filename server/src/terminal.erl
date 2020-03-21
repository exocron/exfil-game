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

-export([start_link/0, send_input/2, init/1, handle_cast/2]).

start_link() -> gen_server:start_link(terminal, self(), []).

send_input(Pid, Input) -> gen_server:cast(Pid, {input, Input}).

init(Pid) ->
    Pid ! {output, <<"QS-DOS 3.13.0\nCopyright (C) 2010-2012 Quantum Softworks, Inc.\n\nC:\\>">>},
    {ok, Pid}.

handle_cast({input, <<>>}, Pid) ->
    Pid ! {output, <<"C:\\>">>},
    {noreply, Pid};

handle_cast({input, Text}, Pid) ->
    Lines = string:lexemes(Text, "\n"),
    [run_command(Pid, parse_command(Line)) || Line <- Lines],
    {noreply, Pid}.

parse_command(Line) ->
    string:lexemes(Line, " \t\r\v\f").

run_command(Pid, []) ->
    Pid ! {output, <<"C:\\>">>};

run_command(Pid, [Cmd | Args]) ->
    case lists:any(fun(X) -> X =:= <<"--help">> orelse X =:= <<"/?">> end, Args) of
        true -> print_help(Pid, Cmd);
        _ -> run_command(Pid, Cmd, Args)
    end.

run_command(Pid, <<"lcontrol.exe">>, Args) ->
    Pid ! {output, <<"\nHi\n\nC:\\>">>};

run_command(Pid, <<"lcontrol">>, Args) ->
    Pid ! {output, <<"\nHi\n\nC:\\>">>};

run_command(Pid, Cmd, _) ->
    Pid ! {output, <<"'", Cmd/binary, "' is not recognized as an internal or external command,\noperable program or batch file.\n\nC:\\>">>}.

print_help(Pid, <<"lcontrol">>) ->
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
