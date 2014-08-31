

-record(hedgy_flow_state, {
    exchange :: record(hedgy_exchange),
    
    controller_mod :: module(),
    controller_state :: any()
}).
