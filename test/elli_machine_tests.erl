-module(elli_machine_tests).
-include_lib("eunit/include/eunit.hrl").

config() ->
    MachineConfig = [
        {dispatcher, {elli_machine_dispatcher, [
            {dispatch_list, [
                %% For get requests
                {[<<"get">>, <<"etag">>], example_controller, [{etag, <<"example-tag">>}]},
                {[<<"get">>, <<"last_modified">>], example_controller, [{last_modified, {{2010, 4, 1}, {10, 30, 51}} } ]},                

                %% For post tests
                {[<<"post">>, '*'], test_post_controller, []},
            
                %% For auth tests
                {[<<"auth">>, <<"realm">>], test_auth_controller, {realm, <<"Basic realm=Drakon">>}},
                {[<<"auth">>, <<"halt">>], test_auth_controller, {halt, 200}}
            ]}
        ]}}
    ], 

    MiddlewareConfig = [{mods, [{elli_machine, MachineConfig}]}],

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

auth_test() ->
    Config = config(),
    ?assertEqual({401, [{<<"WWW-Authenticate">>, <<"Basic realm=Drakon">>}], 
            <<>>},
        elli_test:call('GET', <<"/auth/realm">>, [], <<>>, Config)),

    %% Uses the halt feature which stops processing of the flow.
    ?assertEqual({200, [], <<"halt">>},
        elli_test:call('GET', <<"/auth/halt">>, [], <<>>, Config)),
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


get_error_test() ->
    Config = config(),
    ?assertEqual({405, [{<<"Allow">>, <<"POST">>}], <<>>},
                 elli_test:call('GET', <<"/post">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),
    
    ?assertEqual({404, [], <<"Not Found">>}, elli_test:call('GET', <<"/not_found">>, [], <<>>, Config)),
    ok.

get_etag_test() ->
    Config = config(),

    %% Test if the etag is added.
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, {<<"ETag">>, <<"\"example-tag\"">>}], 
        <<"Hello, new world">>}, 
                      elli_test:call('GET', <<"/get/etag">>, [], <<>>, Config)),

    %% Different etag provided
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, {<<"ETag">>, <<"\"example-tag\"">>}], 
        <<"Hello, new world">>}, 
                      elli_test:call('GET', <<"/get/etag">>, [{<<"If-None-Match">>, <<"\"other\"">>}], <<>>, Config)),

    %% Same tag... should respond with 403
    ?assertEqual({304, [{<<"ETag">>, <<"\"example-tag\"">>}], <<>>}, 
                elli_test:call('GET', <<"/get/etag">>, [{<<"If-None-Match">>, <<"\"example-tag\"">>}], <<>>, Config)),

    ok.

last_modified_test() ->
    Config = config(),

    %% Test if the last-modified header is added
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, 
        {<<"Last-Modified">>, <<"Thu, 01 Apr 2010 10:30:51 GMT">>}], <<"Hello, new world">>}, 
        elli_test:call('GET', <<"/get/last_modified">>, [], <<>>, Config)),

    ok.



