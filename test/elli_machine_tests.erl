-module(elli_machine_tests).
-include_lib("eunit/include/eunit.hrl").

config() ->
    MachineConfig = [
        {dispatcher, {elli_machine_dispatcher, [
            {dispatch_list, [
                {[<<"post">>, '*'], test_post_controller, []},
                {[<<"ws">>, '*'], test_ws_controller, []}
            ]}
        ]}}
    ], 

    MiddlewareConfig = [
        {mods, [
            {elli_machine, MachineConfig}
        ]}
    ],

    [{callback, elli_middleware},
     {callback_args, MiddlewareConfig}, {port, 8000}].

not_found_test() ->
    Config = config(),
    ?assertEqual({404, [], <<"Not Found">>},
                 elli_test:call('GET', <<"/not_found">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),
    ?assertEqual({404, [], <<"Not Found">>}, elli_test:call('GET', <<"/not_found">>, [], <<>>, Config)),
    ok.

post_test() ->
 	Config = config(),
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}], 
            <<"<html><head></head><body>thank-you</body></html>">>},
        elli_test:call('POST', <<"/post">>, [{<<"Host">>, <<"example.com">>}], <<"x=y;">>, Config)),
     ok.

% ws_test() ->
%     Config = config(),
%     ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}], <<"No websocket headers">>},
%         elli_test:call('GET', <<"/ws">>, [{<<"Host">>, <<"example.com">>}], <<>>, Config)),

%     ?assertEqual({101, [], <<>>},
%         elli_test:call('GET', <<"/ws">>, [{<<"Host">>, <<"example.com">>}, 
%             {<<"Upgrade">>, <<"websocket">>}, 
%             {<<"Connection">>, <<"upgrade">>}], <<>>, Config)),
    
%     ok.


get_test() ->
    Config = config(),
    ?assertEqual({405, [{<<"Allow">>, <<"POST">>}], <<>>},
                 elli_test:call('GET', <<"/post">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),
    
    ?assertEqual({404, [], <<"Not Found">>}, elli_test:call('GET', <<"/not_found">>, [], <<>>, Config)),
    ok.
