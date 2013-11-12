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

-include("elli_machine.hrl").

-export_type([machine_request/0]).
-type machine_req() :: record(machine_req).

-export([
	make_machine_req/1, 
	path/1, 
	raw_path/1, 
	args/1
]).

%%
%% Api
%%

-spec make_machine_req(elli:req()) -> machine_req().
make_machine_req(Req) ->
	Host = host(elli_request:headers(Req)),
	#machine_req{req=Req, host=Host}.

path(#machine_req{req=Req}) ->
	elli_request:path(Req).

raw_path(#machine_req{req=Req}) ->
	elli_request:raw_path(Req).

args(#machine_req{req=Req}) ->
	elli_request:args(Req).

%%
%% Helpers
%%

% @doc Return the hostname of the request, or undefined if no host can be found.
-spec host(Headers :: elli:headers()) -> undefined | binary().
host(Headers) ->
    case host_headers(Headers, undefined, undefined, undefined) of
        [] -> undefined;
        [H|_] -> H
    end.

% @doc Return a prioritized list with host header values.
host_headers([], Prio1, Prio2, Prio3) ->
    [V || V <- [Prio1, Prio2, Prio3], V =/= undefined];
host_headers([{<<"X-Forwarded-Host">>, ForwardedHost}|Rest], undefined, Prio2, Prio3) ->
    host_headers(Rest, ForwardedHost, Prio2, Prio3);
host_headers([{<<"X-Forwarded-Server">>, ForwardedServer}|Rest], Prio1, undefined, Prio3) ->
    host_headers(Rest, Prio1, ForwardedServer, Prio3);
host_headers([{<<"Host">>, Host}|Rest], Prio1, Prio2, undefined) ->
    host_headers(Rest, Prio1, Prio2, Host);
host_headers([_|Rest], Prio1, Prio2, Prio3) ->
    host_headers(Rest, Prio1, Prio2, Prio3).

%%
%% Tests
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

host_test() ->
    ?assertEqual(undefined, host([])),

    ?assertEqual(<<"example.com">>, host([{<<"Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Server">>, <<"example.com">>}])),

    ?assertEqual(<<"example.com">>, host([{<<"Content_Length">>, <<"100">>}, {<<"Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Server">>, <<"example.com">>}])),

    % test prio handling
    ?assertEqual(<<"example.com">>, host([
        {<<"X-Forwarded-Server">>, <<"example.com">>}, 
        {<<"Host">>, <<"other.name.com">>} ])),

    ok.

-endif.
