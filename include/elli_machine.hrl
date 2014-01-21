

-record(machine_exchange, {
    req :: undefined | elli:req(),
    req_type = standard :: standard | handover,

    %% Response
    %%
    resp_code = undefined :: undefined | non_neg_integer(),
    resp_headers = [],
    resp_body = <<>> :: binary() | iolist(),

    %% Request Metadata needed during handling the request.
    %%
    host :: undefined | binary(),

    'content-type' :: undefined | binary(),    
    'content-encoding' :: undefined | binary(),
    'chosen-charset' :: undefined | binary(),
    'mediaparams' :: any()
}).

