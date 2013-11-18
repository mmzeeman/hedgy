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

-export([init/2, preprocess/2, handle/2, handle_event/3]).

-export_type([reqdata/0]).
-type reqdata() :: record(machine_reqdata).

-behaviour(elli_handler).

%
% Webmachine like middleware for Elli.
% 


% @doc Initialize the request.
%
init(_Req, _Args) ->
    ignore.
    
% @doc Preprocess the request, call the dispatcher and return our reqdata.
%
preprocess(Req, Args) ->
    {Mod, ModArgs} = dispatcher(Args),
    case Mod:dispatch(Req, ModArgs) of
        {{no_dispatch_match, Host, PathSpec}, ReqData} ->
            ReqData;
        {{ControllerMod, ControllerOpts, 
          HostRemainder, Port, PathRemainder, PathBindings, AppRoot, StringPath}, ReqData} ->
            %% TODO -- fill the rest of the request data strucute.
            ReqData1 = ReqData#machine_reqdata{req=Req},
            {ok, ControllerState} = elli_machine_controller:init(ControllerMod, ControllerOpts),
            ReqData1#machine_reqdata{controller={ControllerMod, ControllerState}}
    end.

% @doc Handle the request.
%
handle(#machine_reqdata{controller=Controller}=ReqData, _Args) ->
    %% Call the decision core
    elli_machine_decision_core:handle_request(Controller, ReqData),

    %% Stop 
    %% elli_machine_controller:stop(ControllerFinState, RequestData)

    %% Respond.
    ignore;

handle(_Req, Args) ->
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

dispatcher(Args) ->
    proplists:get_value(dispatcher, Args).
    

