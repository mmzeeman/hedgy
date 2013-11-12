

-record(machine_req, {
    req :: elli:req(),

    host :: undefined | binary(),

    resp_headers = [],
    resp_body = <<>>
}).
