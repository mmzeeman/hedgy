-module(elli_machine_tests).
-include_lib("eunit/include/eunit.hrl").

config() ->
    MachineConfig = [
        {dispatcher, {elli_machine_dispatcher, [
            {dispatch_list, [
                {[<<"post">>, '*'], post_controller, []}
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
    ?assertEqual({200, [{<<"Content-Encoding">>,<<"identity">>},
                        {<<"Content-Type">>,<<"text/html">>}], <<>>},
                 elli_test:call('POST', <<"/post">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),
    ok.

content_md5_test() ->
    Config = config(),
    ?assertEqual({200, [{<<"Content-Encoding">>,<<"identity">>},
                        {<<"Content-Type">>,<<"text/html">>}], <<>>},
                 elli_test:call('POST', <<"/post">>, 
                    [{<<"Host">>, <<"example.com">>}, 
                     {<<"Content-Md5">>, <<"ulefm731m09zxwDJCm07qw==">>}], 
                    <<"Hoi piepeloi">>, Config)),
    ?assertEqual({400, [], <<>>},
                 elli_test:call('POST', <<"/post">>, 
                    [{<<"Host">>, <<"example.com">>}, 
                     {<<"Content-Md5">>, <<"ulefm631m09zxwDJCm07qw==">>}], 
                    <<"Hoi piepeloi">>, Config)),
    ok.

get_test() ->
    Config = config(),
    ?assertEqual({405, [{<<"Allow">>,"POST"}], <<>>},
                 elli_test:call('GET', <<"/post">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),
    
    ?assertEqual({404, [], <<"Not Found">>}, elli_test:call('GET', <<"/not_found">>, [], <<>>, Config)),
    ok.
