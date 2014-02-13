

-record(machine_exchange, {
    req :: undefined | elli:req(),
    req_type = standard :: standard | handover,

    %% Response
    %%
    resp_code = undefined :: undefined | non_neg_integer(),
    resp_headers = [],
    resp_body = <<>> :: binary() | iolist(),
    resp_is_range_ok = true :: boolean(),

    %% Request Metadata needed during handling the request.
    %%
    host :: undefined | binary(),

    %% 
    content_type :: undefined | binary(),
    content_fun :: undefined | atom(),

    content_encoding :: undefined | binary(),
    chosen_charset :: undefined | binary(),
    
    mediaparams :: any()
}).

