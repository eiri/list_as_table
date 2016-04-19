-module (list_as_table_tests).

-include_lib("eunit/include/eunit.hrl").

empty_test_() ->
  {setup,
    fun() ->
      TmpFile = lib:nonl(os:cmd("mktemp")),
      {ok, IoDevice} = file:open(TmpFile, [read, write]),
      {ok, TmpFile, IoDevice}
    end,
    fun({ok, TmpFile, IoDevice}) ->
      ok = file:close(IoDevice),
      ok = file:delete(TmpFile)
    end,
    fun({ok, TmpFile, IoDevice}) ->
      [
        ?_test(begin
          list_as_table:print(IoDevice, []),
          Expect = <<"+-------------+\n"
            "| empty list  |\n"
            "+-------------+\n"
            "+-------------+\n">>,
          {ok, Table} = file:read_file(TmpFile),
          ?assertEqual(Expect, Table)
        end)
      ]
    end
  }.
