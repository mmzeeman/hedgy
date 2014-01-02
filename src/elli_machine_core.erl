-module(elli_machine_core).

-export(handle_request).

handle_request(Controller, ReqData) ->
    try
        {Ct1, Rd1} = initialize(Controller, ReqData),
        case emr:method(Rd1) of
            'GET' ->

            'POST' ->

            'DELETE' ->

            'PUT' ->

            'OPTIONS' ->

            _ ->
                
        end


initialize(Controller, ReqData) ->
    flow([{{ping, pong}, 503}, 
          {service_available, 503}, 
          {is_known_method, 501},
          {is_method_allowed, 405},
          {{uri_too_long, false}, 412},
          {malformed_request, 400},
          {is_authorized, },
          {forbidden, 403},
          {valid_content_headers, 501},
          {known_content_type, 415},
          {valid_entity_length, 413}].


decision_if({Test, Expected}, Controller, ReqData) ->
    case call(Test, Controller, ReqData) of
        {{respond, _}, _, _}=Respond -> 
            Respond;
        {{error, _}, _, _}=Error -> 
            Error;
        {Expected, Controller1, ReqData1} ->
            {yes, Controller1, ReqData1};
        {_, Controller1, ReqData1} ->
            {no, Controller1, ReqData1}
    end;
decision_if(Test, Controller, ReqData) ->
    decision_if({Test, true}, Controller, ReqData).

flow(Code, Controller, ReqData) ->
    throw({Code, Controller, ReqData})
flow()
flow([], Controller, ReqData) ->
    {continue, Controller, ReqData};
flow([{H, E}|Rest], Controller, ReqData) ->
    case dicision_if(H, Controller, ReqData) of
        {yes, Ct1, Rd1} ->
            flow(Rest, Ct1, Rd1);
        {no, Ct1, Rd1} ->
            flow(E, Ct1, Rd1);
        {{respond, Code}, Ct1, Rd1} ->
            respond(Code, Ct1, Rd1);
        {{error, Code}, Ct1, Rd1} ->
            error_response(Code, Controller, ReqData);
    end.

%% Straight call to the controller.
call(is_known_method, Controller, ReqData) ->
    {Methods, Rs1, Rd1} = controller_call(known_methods, Rs, Rd),
    KnownMethod = lists:member(emr:method(Rd1), Methods),
    {KnownMethod, Rs1, Rd1};

call(is_method_allowed, Controller, ReqData) ->
    {Methods, Rs1, Rd1} = controller_call(allowed_methods, Rs, Rd),
    case lists:member(emr:method(Rd1), Methods) of
        true ->
            {true, Rs1, Rd1};
        false ->
            RdAllow = emr:set_resp_header(<<"Allow">>, string:join([atom_to_list(M) || M <- Methods], ", "), Rd1),
            {false, Rs1, RdAllow};
    end;

call(ControllerCall, Controller, ReqData) ->
    controller_call(ControllerCall, Controller, ReqData).


%%
%% Helpers.
%%

%% @doc Call the controller or a default.
%% @spec controller_call(atom(), Resource, ReqData) -> {term(), NewResource, NewReqData}
controller_call(Fun, Controller, #machine_reqdata{memo=Memo}=ReqData) ->
    case memoize(Fun) of
        true ->
            case proplists:lookup(Fun, Memo) of
                none -> 
                    {T, Controller1, ReqData1} = elli_machine_controller:do(Fun, Controller, ReqData),
                    {T, Controller1, ReqData1#machine_reqdata{memo=[{Fun,T}|Memo]}};
                {Fun, Result} -> 
                    {Result, Controller, ReqData}
            end;
        false ->
            elli_machine_controller:do(Fun, Controller, ReqData)
    end.

memoize(charsets_provided) -> true;
memoize(content_types_provided) -> true;
memoize(encodings_provided) -> true;
memoize(last_modified) -> true;
memoize(generate_etag) -> true;
memoize(_) -> false.

