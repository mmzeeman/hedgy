%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2007-2009 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Decision core for webmachine

%% @doc Adapted for use in elli_machine

-module(elli_machine_decision_core).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Bryan Fink <bryan@basho.com>').

-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').

-export([handle_request/2]).

-include("elli_machine.hrl").

handle_request(Controller, ReqData) ->
    d(v3b13, Controller, ReqData).
    
%% @doc Call the controller or a default.
%% @spec controller_call(atom(), Resource, ReqData) -> {term(), NewResource, NewReqData}
controller_call(Fun, Controller, #machine_reqdata{cache=Cache}=ReqData) ->
    case cacheable(Fun) of
        true ->
            case proplists:lookup(Fun, Cache) of
                none -> 
                    {T, Controller1, ReqData1} = elli_machine_controller:do(Fun, Controller, ReqData),
                    {T, Controller1, ReqData1#machine_reqdata{cache=[{Fun,T}|Cache]}};
                {Fun, Cached} -> 
                    {Cached, Controller, ReqData}
            end;
        false ->
            elli_machine_controller:do(Fun, Controller, ReqData)
    end.

cacheable(charsets_provided) -> true;
cacheable(content_types_provided) -> true;
cacheable(encodings_provided) -> true;
cacheable(last_modified) -> true;
cacheable(generate_etag) -> true;
cacheable(_) -> false.


d(DecisionID, Controller, ReqData) ->
    elli_machine_controller:handle_event(Controller, decision, [DecisionID, ReqData]),
    decision(DecisionID, Controller, ReqData).

respond(Code, Controller, ReqData) ->
    {CtCode, RdCode} = case Code of
        Code when Code =:= 403; Code =:= 404; Code =:= 410 ->
            Reason = {none, none, []},
            {ErrorHTML, CtError, RdError} = 
                elli_machine_controller:render_error(Controller, Code, Reason, ReqData),
            {CtError, emr:set_resp_body(ErrorHTML, RdError)};
        304 ->
            RdNoCT = emr:remove_resp_header(<<"Content-Type">>, ReqData),
            {Etag, CtEt, RdEt0} = controller_call(generate_etag, Controller, RdNoCT),
            RdEt = case Etag of
                undefined -> RdEt0;
                ETag -> emr:set_resp_header(<<"ETag">>, ETag, RdEt0)
            end,
            {Expires, CtExp, RdExp0} = controller_call(expires, CtEt, RdEt),
            RdExp = case Expires of
                undefined -> RdExp0;
                Exp -> emr:set_resp_header(<<"Expires">>, httpd_util:rfc1123_date(calendar:universal_time_to_local_time(Exp)), RdExp0)
            end,
            {CtExp, RdExp};
        _ -> 
            {Controller, ReqData}
    end,
    RdRespCode = emr:set_resp_code(Code, RdCode),
    controller_call(finish_request, CtCode, RdRespCode).

respond(Code, Headers, Rs, Rd) ->
    RdHs = emr:set_resp_headers(Headers, Rd),
    respond(Code, Rs, RdHs).

error_response(Reason, Controller, ReqData) ->
    error_response(500, Reason, Controller, ReqData).

error_response(Code, Reason, Controller, ReqData) ->
    {ErrorHTML, Ct1, Rd1} = elli_machine_controller:render_error(Controller, Code, ReqData, Reason),
    Rd2 = emr:set_resp_body(ErrorHTML, Rd1),
    respond(Code, Ct1, Rd2).

decision_test({Test, Controller, ReqData}, TestVal, TrueFlow, FalseFlow) ->
    decision_test(Test, TestVal, TrueFlow, FalseFlow, Controller, ReqData).

decision_test(Test,TestVal,TrueFlow,FalseFlow, Controller, ReqData) ->
    case Test of
        {error, Reason} ->
            error_response(Reason, Controller, ReqData);
        {error, Reason0, Reason1} -> 
            error_response({Reason0, Reason1}, Controller, ReqData);
        {halt, Code} -> 
            respond(Code, Controller, ReqData);
        TestVal -> 
            decision_flow(TrueFlow, Test, Controller, ReqData);
        _ -> 
            decision_flow(FalseFlow, Test, Controller, ReqData)
    end.

decision_test_fn({Test, Rs, Rd}, TestFn, TrueFlow, FalseFlow) ->
    decision_test_fn(Test, TestFn, TrueFlow, FalseFlow, Rs, Rd).

decision_test_fn({error, Reason}, _TestFn, _TrueFlow, _FalseFlow, Rs, Rd) ->
    error_response(Reason, Rs, Rd);
decision_test_fn({error, R0, R1}, _TestFn, _TrueFlow, _FalseFlow, Rs, Rd) ->
    error_response({R0, R1}, Rs, Rd);
decision_test_fn({halt, Code}, _TestFn, _TrueFlow, _FalseFlow, Rs, Rd) ->
    respond(Code, Rs, Rd);
decision_test_fn(Test,TestFn,TrueFlow,FalseFlow, Rs, Rd) ->
    case TestFn(Test) of
        true -> decision_flow(TrueFlow, Test, Rs, Rd);
        false -> decision_flow(FalseFlow, Test, Rs, Rd)
    end.
    
decision_flow(X, TestResult, Rs, Rd) when is_integer(X) ->
    if X >= 500 -> error_response(X, TestResult, Rs, Rd);
       true -> respond(X, Rs, Rd)
    end;
decision_flow(X, _TestResult, Rs, Rd) when is_atom(X) -> 
    d(X, Rs, Rd);
decision_flow({ErrCode, Reason}, _TestResult, Rs, Rd) when is_integer(ErrCode) ->
    error_response(ErrCode, Reason, Rs, Rd).



%% "Service Available"
decision(v3b13, Rs, Rd) ->
    decision_test(controller_call(ping, Rs, Rd), pong, v3b13b, 503);
decision(v3b13b, Rs, Rd) ->     
    decision_test(controller_call(service_available, Rs, Rd), true, v3b12, 503);
%% "Known method?"
decision(v3b12, Rs, Rd) ->
    {Methods, Rs1, Rd1} = controller_call(known_methods, Rs, Rd),
    decision_test(lists:member(emr:method(Rd1), Methods), true, v3b11, 501, Rs1, Rd1);
%% "URI too long?"
decision(v3b11, Rs, Rd) ->
    decision_test(controller_call(uri_too_long, Rs, Rd), true, 414, v3b10);
%% "Method allowed?"
decision(v3b10, Rs, Rd) ->
    {Methods, Rs1, Rd1} = controller_call(allowed_methods, Rs, Rd),
    case lists:member(emr:method(Rd1), Methods) of
        true ->
            d(v3b9, Rs1, Rd1);
        false ->
            RdAllow = emr:set_resp_header(<<"Allow">>, string:join([atom_to_list(M) || M <- Methods], ", "), Rd1),
            respond(405, Rs1, RdAllow)
    end;
%% "Malformed?"
decision(v3b9, Rs, Rd) ->
    decision_test(controller_call(malformed_request, Rs, Rd), true, 400, v3b8);
%% "Authorized?"
decision(v3b8, Rs, Rd) ->
    {IsAuthorized, Rs1, Rd1} = controller_call(is_authorized, Rs, Rd),
    case IsAuthorized of
        true -> 
            d(v3b7, Rs1, Rd1);
        {error, Reason} ->
            error_response(Reason, Rs1, Rd1);
        {halt, Code}  ->
            respond(Code, Rs1, Rd1);
    AuthHead ->
        RdAuth = emr:set_resp_header(<<"WWW-Authenticate">>, AuthHead, Rd1),
        respond(401, Rs1, RdAuth)
    end;
%% "Forbidden?"
decision(v3b7, Rs, Rd) ->
    decision_test(controller_call(forbidden, Rs, Rd), true, 403, v3b6_upgrade);
%% "Upgrade?"
decision(v3b6_upgrade, Rs, Rd) ->
    case emr:get_req_header_lc(<<"Upgrade">>, Rd) of
                undefined ->
                        d(v3b6, Rs, Rd);
                UpgradeHdr ->
                    case emr:get_req_header_lc(<<"Connection">>, Rd) of
                                undefined ->
                                        d(v3b6, Rs, Rd);
                                Connection ->
                                        case contains_token(<<"upgrade">>, Connection) of
                                                true ->
                                                        {Choosen, Rs1, Rd1} = choose_upgrade(UpgradeHdr, Rs, Rd),
                                                        case Choosen of
                                                                none ->
                                                                        d(v3b6, Rs1, Rd1);
                                                                {_Protocol, UpgradeFunc} ->
                                                                        %% TODO: log the upgrade action
                                                                        {upgrade, UpgradeFunc, Rs1, Rd1}
                                                        end;
                                                false ->
                                                        d(v3b6, Rs, Rd)
                                        end
                        end
        end;
%% "Okay Content-* Headers?"
decision(v3b6, Rs, Rd) ->
    decision_test(controller_call(valid_content_headers, Rs, Rd), true, v3b5, 501);
%% "Known Content-Type?"
decision(v3b5, Rs, Rd) ->
    decision_test(controller_call(known_content_type, Rs, Rd), true, v3b4, 415);
%% "Req Entity Too Large?"
decision(v3b4, Rs, Rd) ->
    decision_test(controller_call(valid_entity_length, Rs, Rd), true, v3b3, 413);
%% "OPTIONS?"
decision(v3b3, Rs, Rd) ->
    case emr:method(Rd) of 
        'OPTIONS' ->
            {Hdrs, Rs1, Rd1} = controller_call(options, Rs, Rd),
            respond(200, Hdrs, Rs1, Rd1);
        _ ->
            d(v3c3, Rs, Rd)
    end;
%% Accept exists?
decision(v3c3, Rs, Rd) ->
    case emr:get_req_header_lc(<<"Accept">>, Rd) of
        undefined ->
            {ContentTypes, Rs1, Rd1} = controller_call(content_types_provided, Rs, Rd),
            PTypes = [Type || {Type,_Fun} <- ContentTypes],
            {ok, RdCT} = emr:set_metadata('content-type', hd(PTypes), Rd1),
            d(v3d4, Rs1, RdCT);
        _ ->
            d(v3c4, Rs, Rd)
    end;
%% Acceptable media type available?
decision(v3c4, Rs, Rd) ->
    {ContentTypesProvided, Rs1, Rd1} = controller_call(content_types_provided, Rs, Rd),
    PTypes = [Type || {Type,_Fun} <- ContentTypesProvided],
    AcceptHdr = emr:get_req_header_lc(<<"Accept">>, Rd1),
    case elli_machine_util:choose_media_type(PTypes, AcceptHdr) of
        none ->
            respond(406, Rs1, Rd1);
        MType ->
            {ok, RdCT} = emr:set_metadata('content-type', MType, Rd1),
            d(v3d4, Rs, RdCT)
    end;
%% Accept-Language exists?
decision(v3d4, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"Accept-Language">>, Rd), undefined, v3e5, v3d5, Rs, Rd);
%% Acceptable Language available? %% WMACH-46 (do this as proper conneg)
decision(v3d5, Rs, Rd) ->
    decision_test(controller_call(language_available, Rs, Rd), true, v3e5, 406);
%% Accept-Charset exists?
decision(v3e5, Rs, Rd) ->
    AcceptCharset = case emr:get_req_header_lc(<<"Accept-Charset">>, Rd) of
        undefined -> <<"*">>;
        Ac -> Ac
    end,
    decision_test(choose_charset(AcceptCharset, Rs, Rd), none, 406, v3f6);    
%% Accept-Encoding exists?
% (also, set content-type header here, now that charset is chosen)
decision(v3f6, Rs, Rd) ->
    CType = emr:get_metadata('content-type', Rd),
    CSet = case emr:get_metadata('chosen-charset', Rd) of
               undefined -> <<"">>;
               CS -> <<"; charset=", CS/binary>>
           end,
    Rd1 = emr:set_resp_header(<<"Content-Type">>, <<CType/binary, CSet/binary>>, Rd),
    case emr:get_req_header_lc(<<"Accept-Encoding">>, Rd1) of
        undefined -> decision_test(choose_encoding(<<"identity;q=1.0,*;q=0.5">>, Rs, Rd1), none, 406, v3g7);
        _ -> d(v3f7, Rs, Rd1)
    end;
%% Acceptable encoding available?
decision(v3f7, Rs, Rd) ->
    decision_test(choose_encoding(emr:get_req_header_lc(<<"Accept-Encoding">>, Rd), Rs, Rd), none, 406, v3g7);
%% "Resource exists?"
decision(v3g7, Rs, Rd) ->
    % this is the first place after all conneg, so set Vary here
    {Variances, Rs1, Rd1} = variances(Rs, Rd),
    RdVar = case Variances of
        [] -> Rd1;
        _ -> emr:set_resp_header(<<"Vary">>, elli_machine_util:binary_join(Variances, <<", ">>), Rd1)
    end,
    decision_test(controller_call(resource_exists, Rs1, RdVar), true, v3g8, v3h7);
%% "If-Match exists?"
decision(v3g8, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"If-Match">>, Rd), undefined, v3h10, v3g9, Rs, Rd);
%% "If-Match: * exists"
decision(v3g9, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"If-Match">>, Rd), <<"*">>, v3h10, v3g11, Rs, Rd);
%% "ETag in If-Match"
decision(v3g11, Rs, Rd) ->
    ETags = elli_machine_util:split_quoted_strings(emr:get_req_header_lc(<<"If-Match">>, Rd)),
    decision_test_fn(controller_call(generate_etag, Rs, Rd),
                     fun(ETag) -> lists:member(ETag, ETags) end,
                     v3h10, 412);
%% "If-Match: * exists"
decision(v3h7, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"If-Match">>, Rd), <<"*">>, 412, v3i7, Rs, Rd);
%% "If-unmodified-since exists?"
decision(v3h10, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"If-Unmodified-Since">>, Rd), undefined, v3i12, v3h11, Rs, Rd);
%% "I-UM-S is valid date?"
decision(v3h11, Rs, Rd) ->
    IUMSDate = emr:get_req_header_lc(<<"If-Unmodified-Since">>, Rd),
    decision_test(elli_machine_util:convert_request_date(IUMSDate), bad_date, v3i12, v3h12, Rs, Rd);
%% "Last-Modified > I-UM-S?"
decision(v3h12, Rs, Rd) ->
    ReqDate = emr:get_req_header_lc(<<"If-Unmodified-Since">>, Rd),
    ReqErlDate = elli_machine_util:convert_request_date(ReqDate),
    {ResErlDate, Rs1, Rd1} = controller_call(last_modified, Rs, Rd),
    decision_test(ResErlDate > ReqErlDate, true, 412, v3i12, Rs1, Rd1);
%% "Moved permanently? (apply PUT to different URI)"
decision(v3i4, Rs, Rd) ->
    {MovedPermanently, Rs1, Rd1} = controller_call(moved_permanently, Rs, Rd),
    case MovedPermanently of
        {true, MovedURI} ->
            RdLoc = emr:set_resp_header(<<"Location">>, MovedURI, Rd1),
            respond(301, Rs1, RdLoc);
        false ->
            d(v3p3, Rs1, Rd1);
        {error, Reason} ->
            error_response(Reason, Rs1, Rd1);
        {halt, Code} ->
            respond(Code, Rs1, Rd1)
    end;
%% PUT?
decision(v3i7, Rs, Rd) ->
    decision_test(emr:method(Rd), 'PUT', v3i4, v3k7, Rs, Rd);
%% "If-none-match exists?"
decision(v3i12, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"If-None-Match">>, Rd), undefined, v3l13, v3i13, Rs, Rd);
%% "If-None-Match: * exists?"
decision(v3i13, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"If-None-Match">>, Rd), <<"*">>, v3j18, v3k13, Rs, Rd);
%% GET or HEAD?
decision(v3j18, Rs, Rd) ->
    decision_test(lists:member(emr:method(Rd),['GET','HEAD']), true, 304, 412, Rs, Rd);
%% "Moved permanently?"
decision(v3k5, Rs, Rd) ->
    {MovedPermanently, Rs1, Rd1} = controller_call(moved_permanently, Rs, Rd),
    case MovedPermanently of
        {true, MovedURI} ->
            RdLoc = emr:set_resp_header(<<"Location">>, MovedURI, Rd1),
            respond(301, Rs1, RdLoc);
        false ->
            d(v3l5, Rs1, Rd1);
        {error, Reason} ->
            error_response(Reason, Rs1, Rd1);
        {halt, Code} ->
            respond(Code, Rs1, Rd1)
    end;
%% "Previously existed?"
decision(v3k7, Rs, Rd) ->
    decision_test(controller_call(previously_existed, Rs, Rd), true, v3k5, v3l7);
%% "Etag in if-none-match?"
decision(v3k13, Rs, Rd) ->
    ETags = elli_machine_util:split_quoted_strings(emr:get_req_header_lc(<<"If-None-Match">>, Rd)),
    decision_test_fn(controller_call(generate_etag, Rs, Rd),
                     %% Membership test is a little counter-intuitive here; if the
                     %% provided ETag is a member, we follow the error case out
                     %% via v3j18.
                     fun(ETag) -> lists:member(ETag, ETags) end,
                     v3j18, v3l13);
%% "Moved temporarily?"
decision(v3l5, Rs, Rd) ->
    {MovedTemporarily, Rs1, Rd1} = controller_call(moved_temporarily, Rs, Rd),
    case MovedTemporarily of
        {true, MovedURI} ->
            RdLoc = emr:set_resp_header(<<"Location">>, MovedURI, Rd1),
            respond(307, Rs1, RdLoc);
        false ->
            d(v3m5, Rs1, Rd1);
        {error, Reason} ->
            error_response(Reason, Rs1, Rd1);
        {halt, Code} ->
            respond(Code, Rs1, Rd1)
    end;
%% "POST?"
decision(v3l7, Rs, Rd) ->
    decision_test(emr:method(Rd), 'POST', v3m7, 404, Rs, Rd);
%% "IMS exists?"
decision(v3l13, Rs, Rd) ->
    decision_test(emr:get_req_header_lc(<<"If-Modified-Since">>, Rd), undefined, v3m16, v3l14, Rs, Rd);
%% "IMS is valid date?"
decision(v3l14, Rs, Rd) -> 
    IMSDate = emr:get_req_header_lc(<<"If-Modified-Since">>, Rd),
    decision_test(elli_machine_util:convert_request_date(IMSDate), bad_date, v3m16, v3l15, Rs, Rd);
%% "IMS > Now?"
decision(v3l15, Rs, Rd) ->
    NowDateTime = calendar:universal_time(),
    ReqDate = emr:get_req_header_lc(<<"If-Modified-Since">>, Rd),
    ReqErlDate = elli_machine_util:convert_request_date(ReqDate),
    decision_test(ReqErlDate > NowDateTime, true, v3m16, v3l17, Rs, Rd);
%% "Last-Modified > IMS?"
decision(v3l17, Rs, Rd) ->
    ReqDate = emr:get_req_header_lc(<<"If-Modified-Since">>, Rd),    
    ReqErlDate = elli_machine_util:convert_request_date(ReqDate),
    {ResErlDate, Rs1, Rd1} = controller_call(last_modified, Rs, Rd),
    decision_test(ResErlDate =:= undefined orelse ResErlDate > ReqErlDate,
                  true, v3m16, 304, Rs1, Rd1);
%% "POST?"
decision(v3m5, Rs, Rd) ->
    decision_test(emr:method(Rd), 'POST', v3n5, 410, Rs, Rd);
%% "Server allows POST to missing resource?"
decision(v3m7, Rs, Rd) ->
    decision_test(controller_call(allow_missing_post, Rs, Rd), true, v3n11, 404);
%% "DELETE?"
decision(v3m16, Rs, Rd) ->
    decision_test(emr:method(Rd), 'DELETE', v3m20, v3n16, Rs, Rd);
%% DELETE enacted immediately?
%% Also where DELETE is forced.
decision(v3m20, Rs, Rd) ->
    decision_test(controller_call(delete_resource, Rs, Rd), true, v3m20b, 500);
decision(v3m20b, Rs, Rd) ->
    decision_test(controller_call(delete_completed, Rs, Rd), true, v3o20, 202);
%% "Server allows POST to missing resource?"
decision(v3n5, Rs, Rd) ->
    decision_test(controller_call(allow_missing_post, Rs, Rd), true, v3n11, 410);
%% "Redirect?"
decision(v3n11, Rs, Rd) ->
    {PostIsCreate, Rs1, Rd1} = controller_call(post_is_create, Rs, Rd),
    {Stage1, RsStage1, RdStage1} = case PostIsCreate of
        true ->
            {CreatePath, Rs2, Rd2} = controller_call(create_path, Rs1, Rd1),
            case CreatePath of
                undefined -> 
                    error_response("post_is_create w/o create_path", Rs2, Rd2);
                NewPath ->
                    case is_list(NewPath) of
                        false -> 
                            error_response("create_path not a string", Rs2, Rd2);
                        true ->
                            {BaseUri0, Rs3, Rd3} = controller_call(base_uri, Rs2, Rd2),
                            BaseUri = case BaseUri0 of
                                            undefined -> 
                                                emr:base_uri(Rd2);
                                            Any ->
                                                case [lists:last(Any)] of
                                                    "/" -> lists:sublist(Any, erlang:length(Any) - 1);
                                                    _ -> Any
                                                 end
                                        end,
                            FullPath = filename:join(["/", emr:path(Rd3), NewPath]),
                            RdPath = emr:set_disp_path(FullPath, Rd3),
                            RdLoc = case emr:get_resp_header("Location", RdPath) of
                                undefined -> emr:set_resp_header("Location", BaseUri ++ FullPath, RdPath);
                                _ -> RdPath
                            end,

                            {Res, Rs3, Rd3} = accept_helper(Rs2, RdLoc),
                            case Res of
                                {respond, Code} -> respond(Code, Rs3, Rd3);
                                {halt, Code} -> respond(Code, Rs3, Rd3);
                                {error, _,_} -> error_response(Res, Rs3, Rd3);
                                {error, _} -> error_response(Res, Rs3, Rd3);
                                _ -> {stage1_ok, Rs3, Rd3}
                            end
                    end
            end;
        _ ->
            {ProcessPost, Rs2, Rd2} = controller_call(process_post, Rs1, Rd1),
            case ProcessPost of
                true -> 
                    {_, Rs3, Rd3} = encode_body_if_set(Rs2, Rd2),
                    {stage1_ok, Rs3, Rd3};
                {halt, Code} -> respond(Code, Rs2, Rd2);
                Err -> error_response(Err, Rs2, Rd2)
            end
    end,
    case Stage1 of
        stage1_ok ->
            case emr:resp_redirect(RdStage1) of
                true ->
                    case emr:get_resp_header(<<"Location">>, RdStage1) of
                        undefined ->
                            respond(500, "Response had do_redirect but no Location", RsStage1, RdStage1);
                        _ ->
                            respond(303, RsStage1, RdStage1)
                    end;
                _ ->
                    d(v3p11, RsStage1, RdStage1)
            end;
        _ -> 
            {nop, RsStage1, RdStage1}
    end;
%% "POST?"
decision(v3n16, Rs, Rd) ->
    decision_test(emr:method(Rd), 'POST', v3n11, v3o16, Rs, Rd);
%% Conflict?
decision(v3o14, Rs, Rd) ->
    {IsConflict, Rs1, Rd1} = controller_call(is_conflict, Rs, Rd),
    case IsConflict of
        true -> respond(409, Rs1, Rd1);
        _ -> 
            {Res, RsHelp, RdHelp} = accept_helper(Rs1, Rd1),
            case Res of
                {respond, Code} -> respond(Code, RsHelp, RdHelp);
                {halt, Code} -> respond(Code, RsHelp, RdHelp);
                {error, _,_} -> error_response(Res, RsHelp, RdHelp);
                {error, _} -> error_response(Res, RsHelp, RdHelp);
                _ -> d(v3p11, RsHelp, RdHelp)
            end
    end;
%% "PUT?"
decision(v3o16, Rs, Rd) ->
    decision_test(emr:method(Rd), 'PUT', v3o14, v3o18, Rs, Rd);
%% Multiple representations?
% (also where body generation for GET and HEAD is done)
decision(v3o18, Rs, Rd) ->    
    BuildBody = case emr:method(Rd) of
        'GET' -> true;
        'HEAD' -> true;
        _ -> false
    end,
    {FinalBody, RsBody, RdBody} = case BuildBody of
        true ->
            {Etag, RsEtag, RdEtag0} = controller_call(generate_etag, Rs, Rd),
            RdEtag = case Etag of
                undefined -> RdEtag0;
                ETag -> emr:set_resp_header(<<"ETag">>, ETag, RdEtag0)
            end,

            {LastModified, RsLM, RdLM0} = controller_call(last_modified, RsEtag, RdEtag),
            RdLM = case LastModified of
                undefined -> RdLM0;
                LM -> emr:set_resp_header(<<"Last-Modified">>, httpd_util:rfc1123_date(calendar:universal_time_to_local_time(LM)), RdLM0)
            end,

            {Expires, RsExp, RdExp0} = controller_call(expires, RsLM, RdLM),
            RdExp = case Expires of
                undefined -> RdExp0;
                Exp -> emr:set_resp_header(<<"Expires">>, httpd_util:rfc1123_date(calendar:universal_time_to_local_time(Exp)), RdExp0)
            end,

            CT = emr:get_metadata('content-type', RdExp),
            {ContentTypesProvided, RsCT, RdCT} = controller_call(content_types_provided, RsExp, RdExp),
            F = hd([Fun || {Type,Fun} <- ContentTypesProvided, CT =:= Type]),
            controller_call(F, RsCT, RdCT);
        false -> 
            {nop, Rs, Rd}
    end,
    case FinalBody of
        {error, _} -> error_response(FinalBody, RsBody, RdBody);
        {error, _,_} -> error_response(FinalBody, RsBody, RdBody);
        {halt, Code} -> respond(Code, RsBody, RdBody);
        nop -> d(v3o18b, RsBody, RdBody);
        _ ->
            {EncodedBody, RsEB, RdEB} = encode_body(FinalBody, RsBody, RdBody), 
            d(v3o18b, RsEB, emr:set_resp_body(EncodedBody, RdEB))
    end;

decision(v3o18b, Rs, Rd) ->
    decision_test(controller_call(multiple_choices, Rs, Rd), true, 300, 200);
%% Response includes an entity?
decision(v3o20, Rs, Rd) ->
    decision_test(emr:has_resp_body(Rd), true, v3o18, 204, Rs, Rd);
%% Conflict?
decision(v3p3, Rs, Rd) ->
    {IsConflict, Rs1, Rd1} = controller_call(is_conflict, Rs, Rd),
    case IsConflict of
        true -> respond(409, Rs1, Rd1);
        _ -> 
            {Res, RsHelp, RdHelp} = accept_helper(Rs1, Rd1),
            case Res of
                {respond, Code} -> respond(Code, RsHelp, RdHelp);
                {halt, Code} -> respond(Code, RsHelp, RdHelp);
                {error, _,_} -> error_response(Res, RsHelp, RdHelp);
                {error, _} -> error_response(Res, RsHelp, RdHelp);
                _ -> d(v3p11, RsHelp, RdHelp)
            end
    end;

%% New controller?  (at this point boils down to "has location header")
decision(v3p11, Rs, Rd) ->
    case emr:get_resp_header(<<"Location">>, Rd) of
        undefined -> d(v3o20, Rs, Rd);
        _ -> respond(201, Rs, Rd)
    end.

accept_helper(Rs, Rd) ->
    CT = case emr:get_req_header_lc(<<"Content-Type">>, Rd) of
             undefined -> <<"application/octet-stream">>;
             Other -> Other
         end,
    {MT, MParams} = elli_machine_util:media_type_to_detail(CT),
    {ok, RdMParams} = emr:set_metadata('mediaparams', MParams, Rd),
    {ContentTypesAccepted, Rs1, Rd1} = controller_call(content_types_accepted, Rs, RdMParams),
    case [Fun || {Type,Fun} <- ContentTypesAccepted, MT =:= Type] of
        [] -> 
            {{respond, 415}, Rs1, Rd1};
        AcceptedContentList ->
            F = hd(AcceptedContentList),
            {Result, Rs2, Rd2} = controller_call(F, Rs1, Rd1),
            case Result of
                true ->
                    {_, RsEncoded, RdEncoded} = encode_body_if_set(Rs2, Rd2),
                    {true, RsEncoded, RdEncoded};
                _ -> 
                    {Result, Rs2, Rd2}
            end
    end.

encode_body_if_set(Rs, Rd) ->
    case emr:has_resp_body(Rd) of
        true ->
            Body = emr:resp_body(Rd),
            {Encoded, Rs1, Rd1} = encode_body(Body, Rs, Rd),
            {true, Rs1, emr:set_resp_body(Encoded, Rd1)};
        _ -> 
            {false, Rs, Rd}
    end.

encode_body(Body, Rs, Rd) ->
    ChosenCSet = emr:get_metadata('chosen-charset', Rd),
    {CharSetsProvided, Rs1, Rd1} = controller_call(charsets_provided, Rs, Rd),
    Charsetter = 
        case CharSetsProvided of
            no_charset -> fun(X) -> X end;
            CP -> hd([Fun || {CSet,Fun} <- CP, ChosenCSet =:= CSet])
        end,
    ChosenEnc = emr:get_metadata('content-encoding', Rd1),
    {EncodingsProvided, Rs2, Rd2} = controller_call(encodings_provided, Rs1, Rd1),
    Encoder = hd([Fun || {Enc,Fun} <- EncodingsProvided, ChosenEnc =:= Enc]),
    %% TODO, change this for elli.
    case Body of
        {stream, StreamBody} ->
            {{stream, make_encoder_stream(Encoder, Charsetter, StreamBody)}, Rs2, Rd2};
        {stream, Size, Fun} ->
            {stream, Size, make_size_encoder_stream(Encoder, Charsetter, Fun)};
        {writer, BodyFun} ->
            {{writer, {Encoder, Charsetter, BodyFun}}, Rs2, Rd2};
        {writer, Size, BodyFun} ->
            {{writer, Size, {Encoder, Charsetter, BodyFun}}, Rs2, Rd2};
        _ ->
            {Encoder(Charsetter(to_binary(Body))), Rs2, Rd2}
    end.
    
    to_binary(Body) when is_tuple(Body) -> Body;
    to_binary(Body) -> iolist_to_binary(Body).

make_size_encoder_stream(Encoder, Charsetter, Fun) ->
    fun(Start, End) ->
        make_encoder_stream(Encoder, Charsetter, Fun(Start, End))
    end.

make_encoder_stream(Encoder, Charsetter, {Body, done}) ->
    {Encoder(Charsetter(Body)), done};
make_encoder_stream(Encoder, Charsetter, {Body, Next}) ->
    {Encoder(Charsetter(Body)), fun() -> make_encoder_stream(Encoder, Charsetter, Next()) end}.

choose_encoding(AccEncHdr, Rs, Rd) ->
    {EncodingsProvided, Rs1, Rd1} = controller_call(encodings_provided, Rs, Rd),
    Encs = [Enc || {Enc,_Fun} <- EncodingsProvided],
    case elli_machine_util:choose_encoding(Encs, AccEncHdr) of
        none -> 
            {none, Rs1, Rd1};
        ChosenEnc ->
        RdEnc = case ChosenEnc of
            "identity" -> Rd1;
            _ -> emr:set_resp_header(<<"Content-Encoding">>, ChosenEnc, Rd1)
        end,
        {ok, RdEnc1} = emr:set_metadata('content-encoding', ChosenEnc, RdEnc),
            {ChosenEnc, Rs1, RdEnc1}
    end.

choose_charset(AccCharHdr, Rs, Rd) ->
    {CharsetsProvided, Rs1, Rd1} = controller_call(charsets_provided, Rs, Rd),
    case CharsetsProvided of
        no_charset ->
            {no_charset, Rs1, Rd1};
        CL ->
            CSets = [CSet || {CSet,_Fun} <- CL],
            case elli_machine_util:choose_charset(CSets, AccCharHdr) of
                none -> 
                    {none, Rs1, Rd1};
                Charset ->
                    {ok, RdCSet} = emr:set_metadata('chosen-charset', Charset, Rd1),
                    {Charset, Rs1, RdCSet}
            end
    end.

choose_upgrade(UpgradeHdr, Rs, Rd) ->
    {UpgradesProvided, Rs1, Rd1} = controller_call(upgrades_provided, Rs, Rd),
        Provided1 = [ {string:to_lower(Prot), Prot, PFun} || {Prot, PFun} <- UpgradesProvided],
        Requested = [ string:to_lower(string:strip(Up)) || Up <- string:tokens(UpgradeHdr, ",") ],
        {choose_upgrade1(Requested, Provided1), Rs1, Rd1}.

        choose_upgrade1([], _) ->
                none;
        choose_upgrade1([Req|Requested], Provided) ->
                case lists:keysearch(Req, 1, Provided) of
                        false ->
                                choose_upgrade1(Requested, Provided);
                        {value, {_, Protocol, UpgradeFun}} ->
                                {Protocol, UpgradeFun}
                end.


variances(Rs, Rd) ->
    {ContentTypesProvided, Rs1, Rd1} = controller_call(content_types_provided, Rs, Rd),
    Accept = case length(ContentTypesProvided) of
        1 -> [];
        0 -> [];
        _ -> [<<"Accept">>]
    end,
    {EncodingsProvided, Rs2, Rd2} = controller_call(encodings_provided, Rs1, Rd1),
    AcceptEncoding = case length(EncodingsProvided) of
        1 -> [];
        0 -> [];
        _ -> [<<"Accept-Encoding">>]
    end,
    {CharsetsProvided, Rs3, Rd3} = controller_call(charsets_provided, Rs2, Rd2),
    AcceptCharset = case CharsetsProvided of
        no_charset -> 
            [];
        CP ->
            case length(CP) of
                1 -> [];
                0 -> [];
                _ -> [<<"Accept-Charset">>]
            end
    end,
    {Variances, Rs4, Rd4} = controller_call(variances, Rs3, Rd3),
    {Accept ++ AcceptEncoding ++ AcceptCharset ++ Variances, Rs4, Rd4}.
    
    
contains_token(Token, HeaderString) ->
    NoWsHeaderString = elli_machine_util:remove_whitespace(HeaderString),
    Tokens = binary:split(NoWsHeaderString, <<",">>, [global]),
    lists:member(Token, Tokens).
