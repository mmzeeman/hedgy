%%
%%

-module(elli_machine_flow_tests).
-include_lib("eunit/include/eunit.hrl").

can_range_test() ->
    ?assertEqual(true, elli_machine_flow:can_range(undefined, undefined, undefined)),
    ?assertEqual(true, elli_machine_flow:can_range(<<"\"etag\"">>, undefined, undefined)),
    ?assertEqual(true, elli_machine_flow:can_range(<<"etag">>, undefined, <<"\"etag\"">>)),

    %% if-range is matched as date, doesn't match so we can't range.
    ?assertEqual(false, elli_machine_flow:can_range(undefined, undefined, <<"different-stuff">>)),
   
    %% we can't range, for safety sake there is a bad date in the if-range header.
    ?assertEqual(false, elli_machine_flow:can_range(undefined, {{1970,11,8}, {8,0,0}}, <<"different-stuff">>)),

    %% There is no if-range header, so we can range.
    ?assertEqual(true, elli_machine_flow:can_range(undefined, {{1970,11,8}, {8,0,0}}, undefined)),

    ?assertEqual(false, elli_machine_flow:can_range(undefined, {{1970,11,8}, {8,0,0}}, <<"Sun, 08 Nov 1969 08:00:00 GMT">>)),
    ?assertEqual(true, elli_machine_flow:can_range(undefined, {{1970,11,8}, {8,0,0}}, <<"Sun, 08 Nov 1970 08:00:01 GMT">>)),


    %% assertEqual(<<"Sun, 08 Nov 1970 08:00:00 GMT">>, rfc1123_date({{1970,11,8}, {8,0,0}})),


    %% etags differ
    ?assertEqual(false, elli_machine_flow:can_range(<<"etag">>, undefined, <<"\"other\"">>)),

    ok.