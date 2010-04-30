-module(util).

-compile([export_all]).

format(true,Fmt_string, Arg) ->
    Msg = lists:flatten(io_lib:format(Fmt_string, Arg)),
    Pid = self(),
    [{registered_name, Name}] = process_info(Pid, [registered_name]),
    io:format("{~p,~p,~p}~s", [now(),Pid,Name,Msg]).

format(true,Fmt_string, Arg, {Module, Line}) ->
    Msg = lists:flatten(io_lib:format(Fmt_string, Arg)),
    Pid = self(),
    [{registered_name, Name}] = process_info(Pid, [registered_name]),
    io:format("{~p,~p,~p,~p,~p}~s", [now(),Module,Line,Pid,Name,Msg]);
format(false , _Fmt_string, _Arg, _Options) ->
    true.
