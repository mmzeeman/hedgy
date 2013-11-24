
-module(test_machine).

-export([start/0]).

start() ->
    _ = application:start(crypto),
    _ = application:start(public_key),
    _ = application:start(ssl),
    
    MachineConfig = [
        {dispatcher, {elli_machine_dispatcher, [
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
            {elli_machine, MachineConfig}
        ]}
    ],

    elli:start_link([{callback, elli_middleware},
                     {callback_args, Config}, {port, 8000}]).
