

-record(machine_reqdata, {
	host :: undefined | binary(),

    cache = [],

    resp_headers = [], 
    resp_body = <<>> :: binary() | iolist(),

    controller, 
    req :: undefined | elli:req()
}).

