%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
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

-export([init/2, handle/2, handle_event/3]).

-export_type([reqdata/0]).
-type reqdata() :: record(machine_reqdata).

-behaviour(elli_handler).

% @doc Initialize the request.
%
init(Req, Args) ->
    {Mod, ModArgs} = proplists:get_value(dispatcher, Args),
    case Mod:dispatch(Req, ModArgs) of
        {{no_dispatch_match, Host, PathSpec}, ReqData} ->
            io:fwrite(standard_error, "no_match: ~p~n", [ModArgs]),
            ignore;
        {{ControllerMod, ControllerOpts, 
          HostRemainder, Port, PathRemainder, PathBindings, AppRoot, StringPath}, ReqData} ->
            %% TODO -- fill the rest of the request data strucute.
            ReqData1 = ReqData#machine_reqdata{req=Req},

            io:fwrite(standard_error, "mod: ~p~n", [ControllerMod]),
            {ok, standard, ReqData1#machine_reqdata{controller={ControllerMod, ControllerOpts}}}
    end.

% @doc Handle a request.
%
handle(Req, #machine_reqdata{controller={Mod, ModOpts}}=ReqData) ->
    %% Initialize the controller.
    {ok, ControllerState} = elli_machine_controller:init(Mod, ModOpts),

    %% Call the decision core
    elli_machine_decision_core:handle_request({Mod, ControllerState}, ReqData),

    io:fwrite(standard_error, "handle: ~p~n", [Req]),

    %% Stop 
    %% elli_machine_controller:stop(ControllerFinState, RequestData)

    %% Respond.
    ignore;

handle(_,_) ->
    ignore.
            
    
% @doc Handle event
%
handle_event(elli_startup, [], Config) ->
   ok; 

% Errors during request handlers
handle_event(request_throw, [_Request, _Exception, _Stacktrace]=E, _) ->
    report(request_throw, E),
    ok;
handle_event(request_error, [_Request, _Exception, _Stacktrace]=E, _) -> 
    report(request_error, E),
    ok;
handle_event(request_exit, [_Request, _Exception, _Stacktrace]=E, _) -> 
    report(request_exit, E),
    ok;

% Other events.
handle_event(_Name, _EventArgs, _) -> ok.

%%
%% Helpers
%%

report(Name, Term) ->
    io:fwrite(standard_error, "~p: ~p~n", [Name, Term]).
    

