

-record(machine_flow_state, {
    exchange :: record(machine_exchange),
    
    controller_mod :: module(),
    controller_state :: any()
}).