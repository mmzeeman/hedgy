

-type some(Type) :: undefined | {some, Type}.

-record(machine_reqdata, {
	req :: undefined | elli:req(),
	host :: undefined | binary(),

    cache = [],

    resp_headers = [], 
    resp_body = <<>> :: binary() | iolist(),

    controller, 
    
    %% Request Metadata
    'content-type' :: undefined | binary(),    
    'content-encoding' :: undefined | binary(),
    'chosen-charset' :: undefined | binary(),
    'mediaparams' :: any()
}).

