%% @doc list_as_table a simple way to present a list of terms as a table
%% @version 0.1
%% @reference <a href="https://github.com/eiri/as_table">https://github.com/eiri/as_table</a>
%% @author Eric Avdey <eiri@eiri.ca>
%% @copyright 2014 Eric Avdey

-module(list_as_table).

-author('Eric Avdey <eiri@eiri.ca>').

-export([print/1]).

-type proplist() :: [tuple()].

%% @doc Prints list of proplists as a table
-spec print([proplist()]) -> ok.
print(List) ->
  {ok, Headers, Sizes} = measure_all(List),
  V = fun(L) -> [V || {_, V} <- L] end,
  print_line(Sizes),
  print_row(Headers, Sizes),
  print_line(Sizes),
  [print_row(V(L), Sizes) || L <- List],
  print_line(Sizes).

%% private

print_row(R, S) ->
  print_row(R, S, "").

print_row([V], [S], L) ->
  io:format("~s |~n", [L ++ print_cell(V,S)]);
print_row([V|T], [S|ST], L) ->
  print_row(T, ST, L ++ print_cell(V,S)).

print_cell(V,S) ->
  FmtS = lists:flatten(io_lib:format("| ~~-~bs ", [S])),
  FmtV = if
    is_list(V) -> io_lib:format("~s", [V]);
    is_integer(V) -> io_lib:format("~b", [V]);
    true -> io_lib:format("~p", [V])
  end,
  io_lib:format(FmtS, [list_to_binary(FmtV)]).

print_line(S) ->
  print_line(S, "").

print_line([], L) ->
  io:format("~s-+~n", [L]);
print_line([S|ST], L) ->
  FmtS = lists:flatten(io_lib:format("+~~~bc", [S+2])),
  print_line(ST, L ++ io_lib:format(FmtS, [$-])).

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