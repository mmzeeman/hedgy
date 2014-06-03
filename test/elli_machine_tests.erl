-module(elli_machine_tests).
-include_lib("eunit/include/eunit.hrl").

config() ->
    MachineConfig = [
        {dispatcher, {elli_machine_dispatcher, [
            {dispatch_list, [
                %% For get requests
                {[<<"get">>, <<"etag">>], example_controller, 
                    [{etag, <<"example-tag">>}]},
                {[<<"get">>, <<"last_modified">>], example_controller, 
                    [{last_modified, {{2010, 4, 1}, {10, 30, 51}} } ]},                

                %% Conneg tests
                {[<<"get">>, <<"conneg">>], test_conneg_controller, []}, 

                %% For post tests
                {[<<"post">>, '*'], test_post_controller, []},
            
                %% For auth tests
                {[<<"auth">>, <<"realm">>], test_auth_controller, 
                    {realm, <<"Basic realm=Drakon">>}},
                {[<<"auth">>, <<"halt">>], test_auth_controller, 
                    {halt, 200}}
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
    ?assertEqual({200, [
                {<<"Content-Type">>,<<"text/html">>}, 
                {<<"ETag">>, <<"\"example-tag\"">>}], 
            <<"Hello, new world">>}, 
        elli_test:call('GET', <<"/get/etag">>, [], <<>>, Config)),

    %% Different etag provided
    ?assertEqual({200, [
                {<<"Content-Type">>,<<"text/html">>}, 
                {<<"ETag">>, <<"\"example-tag\"">>}], 
            <<"Hello, new world">>}, 
        elli_test:call('GET', <<"/get/etag">>, 
            [{<<"If-None-Match">>, <<"\"other\"">>}], <<>>, Config)),

    %% Same tag... should respond with 403
    ?assertEqual({304, [
                {<<"ETag">>, <<"\"example-tag\"">>}], <<>>}, 
                elli_test:call('GET', <<"/get/etag">>, 
                    [{<<"If-None-Match">>, <<"\"example-tag\"">>}], <<>>, Config)),

    ok.

last_modified_test() ->
    Config = config(),

    %% Test if the last-modified header is added
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, 
        {<<"Last-Modified">>, <<"Thu, 01 Apr 2010 10:30:51 GMT">>}], <<"Hello, new world">>}, 
        elli_test:call('GET', <<"/get/last_modified">>, [], <<>>, Config)),

    ok.

if_modified_since_test() ->
    Config = config(),

    %% Wrong date format... ignore...
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, 
        {<<"Last-Modified">>, <<"Thu, 01 Apr 2010 10:30:51 GMT">>}], <<"Hello, new world">>}, 
        elli_test:call('GET', <<"/get/last_modified">>, 
            [{<<"If-Modified-Since">>, <<"yeah">>}], <<>>, Config)),

    %% 
    ?assertEqual({304, [], <<>>}, 
        elli_test:call('GET', <<"/get/last_modified">>, 
            [{<<"If-Modified-Since">>, <<"Thu, 02 Apr 2010 10:30:50 GMT">>}], <<>>, Config)),

    %% Wrong date format should be ignored.
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, 
        {<<"Last-Modified">>, <<"Thu, 01 Apr 2010 10:30:51 GMT">>}], <<"Hello, new world">>}, 
        elli_test:call('GET', <<"/get/last_modified">>, 
            [{<<"If-Modified-Since">>, <<"Thu, 02 Apr 2010 10:30:70 GMT">>}], <<>>, Config)),

    ok.

content_negotiation_test() ->
    Config = config(),

    %% Check if the vary header is set.
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, {<<"Vary">>, <<"Accept">>}], 
            <<"<html><head></head><body>html response</body></html>">>}, 
        elli_test:call('GET', <<"/get/conneg">>, [], <<>>, Config)),

    %% Requesting text plain should result in a plain text response. 
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/plain">>}, {<<"Vary">>, <<"Accept">>}], 
            <<"plain response">>}, 
        elli_test:call('GET', <<"/get/conneg">>, [{<<"Accept">>, <<"text/plain">>}], <<>>, Config)),

    %% Requesting javascript, not-acceptable.
    ?assertEqual({406, [], <<>>}, 
        elli_test:call('GET', <<"/get/conneg">>, [{<<"Accept">>, <<"text/javascript">>}], <<>>, Config)),

    %% Client accepts html and plain responses, but likes plain responses better.
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/plain">>}, {<<"Vary">>, <<"Accept">>}], 
            <<"plain response">>}, 
        elli_test:call('GET', <<"/get/conneg">>, [
                {<<"Accept">>, <<"text/plain; q=0.5, text/html; q=0.2">>}], <<>>, Config)),

    %% Client accepts html and plain responses, but likes plain responses better.
    %% With multiple accept headers
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/plain">>}, {<<"Vary">>, <<"Accept">>}], 
            <<"plain response">>}, 
        elli_test:call('GET', <<"/get/conneg">>, [
                {<<"Accept">>, <<"text/plain; q=0.5">>}, 
                {<<"Accept">>, <<"text/html; q=0.2">>}], <<>>, Config)),

    %% Client accepts html and plain responses, but likes html responses better.
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, {<<"Vary">>, <<"Accept">>}], 
            <<"<html><head></head><body>html response</body></html>">>}, 
        elli_test:call('GET', <<"/get/conneg">>, [
                {<<"Accept">>, <<"text/plain; q=0.1, text/html; q=0.5">>}], <<>>, Config)),

    %% Client accepts html and plain responses, but likes html responses better.
    %% With multiple accept headers
    ?assertEqual({200, [{<<"Content-Type">>,<<"text/html">>}, {<<"Vary">>, <<"Accept">>}], 
            <<"<html><head></head><body>html response</body></html>">>}, 
        elli_test:call('GET', <<"/get/conneg">>, [
                {<<"Accept">>, <<"text/plain; q=0.1">>}, 
                {<<"Accept">>, <<"text/html; q=0.5">>}], <<>>, Config)),

    ok.

    



