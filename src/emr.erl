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

-module(emr).

-include_lib("elli/include/elli.hrl").
-include("elli_machine.hrl").

-export([
    make_reqdata/1,

    method/1,
    get_req_header_lc/2,

    set_resp_header/3,
    set_metadata/3,
    get_metadata/2
]).

%%
%% Api
%%

-spec make_reqdata(Host :: undefined | binary()) -> elli_machine:req().
make_reqdata(Host) ->
    #machine_reqdata{host=Host}.

% @doc 
method(#machine_reqdata{req=Req}) ->
    elli_request:method(Req).

% @doc
get_req_header_lc(Header, #machine_reqdata{req=Req}) ->
    case elli_request:get_header(Header, Req) of
        undefined -> 
            undefined;
        Val ->
            elli_bstr:to_lower(Val)
    end.

set_resp_header(Header, Value, #machine_reqdata{resp_headers=RespHeaders}=ReqData) ->
    ReqData#machine_reqdata{resp_headers=[{Header, Value}|RespHeaders]}.


% @doc Sets metadata
set_metadata('content-type', Val, ReqData) ->
    {ok, ReqData#machine_reqdata{'content-type' = Val}};
set_metadata('content-encoding', Val, ReqData) ->
    {ok, ReqData#machine_reqdata{'content-encoding' = Val}};
set_metadata('chosen-charset', Val, ReqData) ->
    {ok, ReqData#machine_reqdata{'chosen-charset' = Val}};
set_metadata('mediaparams', Val, ReqData) ->
    {ok, ReqData#machine_reqdata{'mediaparams' = Val}}.

% @doc Get metadata.
get_metadata('content-type', ReqData) ->
    ReqData#machine_reqdata.'content-type';
get_metadata('content-encoding', ReqData) ->
    ReqData#machine_reqdata.'content-encoding';
get_metadata('chosen-charset', ReqData) ->
    ReqData#machine_reqdata.'chosen-charset';
get_metadata('mediaparams', ReqData) ->
    ReqData#machine_reqdata.'mediaparams'.





