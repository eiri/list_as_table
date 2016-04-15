%% @doc list_as_table a simple way to present a list of terms as a table
%% @version 0.1
%% @reference <a href="https://github.com/eiri/as_table">https://github.com/eiri/as_table</a>
%% @author Eric Avdey <eiri@eiri.ca>
%% @copyright 2014 Eric Avdey

-module(list_as_table).

-author('Eric Avdey <eiri@eiri.ca>').

-export([print/1, print/2, values/1]).

-type device() :: atom() | pid().
-type proplist() :: [tuple()].

%% @doc Prints list of proplists as a table
-spec print([proplist()]) -> ok.
print(List) ->
  print(standard_io, List).

%% @doc Prints list of proplists as a table on a provided IO device.
%% Device can be standard_io, standard_error, a registered name,
%% or a pid handling IO protocols as returned by file:open/2.
-spec print(device(), [proplist()]) -> ok.
print(Device, List) ->
  {ok, Headers, Sizes} = measure_all(List),
  Line = format_line(Sizes),
  Header = format_row(Headers, Sizes),
  Rows = [format_row(values(E), Sizes) || E <- List],
  lists:foreach(fun(Row) ->
    io:fwrite(Device, Row, [])
  end, lists:append([[Line, Header, Line], Rows, [Line]])).

values(List) ->
  lists:map(fun({_, V}) -> V end, List).

%% private

format_row(R, S) ->
  format_row(R, S, "").

format_row([V], [S], L) ->
  io_lib:format("~s |~n", [L ++ format_cell(V,S)]);
format_row([V|T], [S|ST], L) ->
  format_row(T, ST, L ++ format_cell(V,S)).

format_cell(V,S) ->
  FmtS = list_to_binary([$|, " ", $~, $-, integer_to_list(S), $s, " "]),
  FmtV = if
    is_list(V) -> io_lib:format("~s", [V]);
    is_integer(V) -> io_lib:format("~b", [V]);
    is_float(V) -> io_lib:format("~f", [V]);
    true -> io_lib:format("~P", [V, 5])
  end,
  io_lib:format(FmtS, [list_to_binary(FmtV)]).

format_line(S) ->
  format_line(S, "").

format_line([], L) ->
  io_lib:format("~s-+~n", [L]);
format_line([S|ST], L) ->
  FmtS = list_to_binary([$+, $~, integer_to_list(S + 2), $c]),
  format_line(ST, L ++ io_lib:format(FmtS, [$-])).

measure_all([H|_] = List) ->
  Mold = [0 || _ <- lists:seq(1, length(H))],
  Headers = [K || {K,_} <- H],
  Sizes = measure_all(List, Mold),
  {ok, Headers, Sizes}.

measure_all([], Acc) ->
  Acc;
measure_all([H|T], Acc) ->
  measure_all(T, measure_row(H, Acc, [])).

measure_row([], [], NewAcc) ->
  lists:reverse(NewAcc);
measure_row([{K,V}|RowT], [Size|AccT], NewAcc) ->
  KSize = fsize(K),
  case fsize(V) of
    VSize when VSize > Size andalso VSize > KSize ->
      measure_row(RowT, AccT, [VSize|NewAcc]);
    VSize when VSize > Size ->
      measure_row(RowT, AccT, [KSize|NewAcc]);
    _ ->
      measure_row(RowT, AccT, [Size|NewAcc])
  end.

fsize(V) when is_list(V) ->
  length(V);
fsize(V) when is_binary(V) ->
  erlang:size(V);
fsize(V) ->
  erlang:size(list_to_binary(io_lib:format("~p", [V]))).
