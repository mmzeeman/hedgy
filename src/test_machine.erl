
-module(test_machine).

-export([start/0]).

start() ->
    _ = application:start(crypto),
    _ = application:start(public_key),
    _ = application:start(ssl),
    
    HedgyConfig = [
        {dispatcher, {hedgy_dispatcher, [
            {dispatch_list, [
                {[<<"hello">>, '*'], example_controller, []}
            ]}
        ]}}
    ], 

    Config = [
        {mods, [
            {elli_debug, []},
            {elli_date, []},
            {elli_server_name, []},
            {hedgy, HedgyConfig}
        ]}
    ],

    elli:start_link([{callback, elli_middleware},
                     {callback_args, Config}, {port, 8000}]).
