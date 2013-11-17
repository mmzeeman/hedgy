
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

    elli:start_link([{callback, elli_machine},
                     {callback_args, MachineConfig}, {port, 8000}]).
