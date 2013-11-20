

-type some(Type) :: undefined | {some, Type}.

-record(machine_reqdata, {
	req :: undefined | elli:req(),

	host :: undefined | binary(),

    cache = [],

    resp_code = undefined :: undefined | non_neg_integer(),
    resp_headers = [], 
    resp_body = <<>> :: binary() | iolist(),

    %% Request Metadata
    'content-type' :: undefined | binary(),    
    'content-encoding' :: undefined | binary(),
    'chosen-charset' :: undefined | binary(),
    'mediaparams' :: any()
}).

