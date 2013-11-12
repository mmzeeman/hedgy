%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012, 2013 Maas-Maarten Zeeman
%%
%% @doc Elli Machine 
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

-module(elli_machine).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-include_lib("elli/include/elli.hrl").
-include("elli_machine.hrl").

-export([handle/2, handle_event/3]).

-export_type([req/0]).
-type req() :: record(machine_req).

-behaviour(elli_handler).

-record(machine_config, {
    dispatcher :: module(),
    dispatch_args :: any()
}).

-type dispatcher() :: any().
-type dispatcher_args() :: any().

% @doc Handle a request.
%
handle(Req, Args) ->
    ReqData = emr:make_machine_req(Req),
    {Dispatch, DispatchArgs} = dispatch(ReqData, Args),
    handle_request(Dispatch, DispatchArgs).

% @doc Handle event
%
handle_event(elli_startup, [], Config) ->
   % elli_machine_server:start_link(Config),
   ok; 

% Errors during request handlers
handle_event(request_throw, [_Request, _Exception, _Stacktrace], _) -> ok;
handle_event(request_error, [_Request, _Exception, _Stacktrace], _) -> ok;
handle_event(request_exit, [_Request, _Exception, _Stacktrace], _) -> ok;

% Other events.
handle_event(_Name, _EventArgs, _) -> ok.

%%
%% Helpers
%%

-spec handle_request(dispatcher(), dispatcher_args()) -> ignore.  
handle_request(_, _) ->
    ignore.

% @doc Find a matching controller for this request.
-spec dispatch(Rd :: req(), Config :: any()) -> any().
dispatch(Rd, Args) ->
    case application:get_env(?MODULE, dispatcher, elli_machine_dispatcher) of
        undefined -> 
            elli_machine_dispatcher:dispatch(Rd, Args);
        Dispatcher ->
            Dispatcher:dispatch(Rd, Args)
    end.
