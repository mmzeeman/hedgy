%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%%
%% @doc Elli Machine Request 
%%
%% Copyright 2013 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% Short for elli machine exchange.
-module(emx).

-include_lib("elli/include/elli.hrl").
-include("elli_machine.hrl").

-export([
    make_exchange/1,

    response/1,

    method/1,

    is_get_or_head/1,

    get_req_body/1,

    get_req_header/2,
    get_req_header_lc/2,

    set_resp_code/2,

    set_resp_header/3,
    set_resp_headers/2,
    get_resp_header/2,

    get_resp_content_type/1,
    set_resp_content_type/2,
    
    get_resp_content_fun/1,
    set_resp_content_fun/2,

    get_resp_chosen_charset/1,
    set_resp_chosen_charset/2,

    set_resp_body/2,
    get_resp_body/1,

    set_resp_range/2,

    has_resp_body/1,

    set_metadata/3,
    get_metadata/2
]).

%%
%% Api
%%

-spec make_exchange(Req :: elli:req()) -> elli_machine:exchange().
make_exchange(Req) ->
    #machine_exchange{req=Req}.

% @doc Return an elli response.
response(#machine_exchange{resp_code=Code, resp_headers=Headers, resp_body=Body}) 
        when Code =/= undefined ->
    {Code, Headers, Body}.


%%
%% @doc Call the controller or a default.
%% @spec controller_call(atom(), Resource, ReqData) -> {term(), NewResource, NewReqData}
controller_do(Fun, #machine_exchange{}=ReqData) ->
    elli_machine_controller:do(ReqData).


% @doc Return the method of the request.
method(#machine_exchange{req=Req}) ->
    elli_request:method(Req).

% @doc Return true iff this is a GET or HEAD request.
is_get_or_head(#machine_exchange{req=Req}) ->
    case elli_request:method(Req) of
        'GET' -> 
            true;
        'HEAD' -> 
            true;
        _ -> 
            false
    end.

% @doc Get the request body.
get_req_body(#machine_exchange{req=Req}) ->
    elli_request:body(Req).

% @doc Gte
get_req_header(Header, #machine_exchange{req=Req}) ->
    elli_request:get_header(Header, Req).
    
% @doc
get_req_header_lc(Header, ReqData) ->
    case get_req_header(Header, ReqData) of
        undefined -> undefined;
        Val -> elli_bstr:to_lower(Val)
    end.

% @doc Set the response code of the request
set_resp_code(Code, Exchange) ->
    Exchange#machine_exchange{resp_code=Code}.

% @doc Set a response header.
set_resp_header(Header, Value, #machine_exchange{resp_headers=RespHeaders}=Exchange) ->
    Exchange#machine_exchange{resp_headers=[{Header, Value}|RespHeaders]}.

% @doc Set the headers.
set_resp_headers(Headers, #machine_exchange{resp_headers=RespHeaders}=Exchange) ->
    Exchange#machine_exchange{resp_headers=RespHeaders++Headers}.

% @doc Get a response header
get_resp_header(Key, #machine_exchange{resp_headers=RespHeaders}) ->
    proplists:get_value(Key, RespHeaders).

% @doc Set the response body.
set_resp_body(Body, ReqData) ->
    ReqData#machine_exchange{resp_body=Body}.

% @doc Get the response body.
get_resp_body(ReqData) ->
    ReqData#machine_exchange.resp_body.

% @doc Returns true iff the request has a response body.
-spec has_resp_body(elli_machine:exchange()) -> false.
has_resp_body(#machine_exchange{resp_body= <<>>}) ->
    false;
has_resp_body(_) ->
    true.

set_resp_content_type(Val, Exchange) ->
    Exchange#machine_exchange{content_type=Val}.

get_resp_content_type(Exchange) ->
    Exchange#machine_exchange.content_type.

set_resp_content_fun(Val, Exchange) ->
    Exchange#machine_exchange{content_fun=Val}.

get_resp_content_fun(Exchange) ->
    Exchange#machine_exchange.content_fun.

set_resp_chosen_charset(Val, Exchange) ->
    Exchange#machine_exchange{chosen_charset=Val}.

get_resp_chosen_charset(Exchange) ->
    Exchange#machine_exchange.chosen_charset.

set_resp_range(Range, Exchange) ->
    Exchange#machine_exchange{range=Range}.




% @doc Sets metadata
set_metadata('content-encoding', Val, Exchange) ->
    {ok, Exchange#machine_exchange{content_encoding = Val}};
set_metadata('chosen-charset', Val, Exchange) ->
    {ok, Exchange#machine_exchange{chosen_charset = Val}};
set_metadata('mediaparams', Val, Exchange) ->
    {ok, Exchange#machine_exchange{mediaparams = Val}}.

% @doc Get metadata.
get_metadata(content_encoding, Exchange) ->
    Exchange#machine_exchange.content_encoding;
get_metadata(chosen_charset, Exchange) ->
    Exchange#machine_exchange.chosen_charset;
get_metadata(mediaparams, Exchange) ->
    Exchange#machine_exchange.mediaparams.





