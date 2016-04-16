-module (list_as_table_tests).

-include_lib("eunit/include/eunit.hrl").

main_test() ->
  ?debugVal( list_as_table:print([[{a, 1}]]) ),
  ?assert(true).
