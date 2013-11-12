

-record(machine_reqdata, {
    req :: elli:req(),

    host :: undefined | binary(),

    resp_headers = [],
    resp_body = <<>>
}).
