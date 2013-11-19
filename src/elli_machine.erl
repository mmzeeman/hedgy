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

-export([preprocess/2, postprocess/3, handle/2, handle_event/3]).

-export_type([reqdata/0]).
-type reqdata() :: record(machine_reqdata).

-behaviour(elli_handler).

%
% Webmachine like middleware for Elli.
% 
    
% @doc Preprocess the request, call the dispatcher and return a controller 
% reqdata.
%
-spec preprocess(elli:req(), any()) -> {{module(), any()}, reqdata()}.
preprocess(Req, Args) ->
    {Mod, ModArgs} = dispatcher(Args),
    dispatch(Req, Mod, ModArgs).

% @doc Handle the request. Call the decision core which calls the callbacks
% of the controller.
handle({Controller, ReqData}, _Args) when Controller =/= undefined ->
    case elli_machine_decision_core:handle_request(Controller, ReqData) of
        {_, ControllerFin, ReqDataFin} ->                 
            emr:response(ReqDataFin);
        {upgrade, _UpgradeFun, _ControllerFin, _ReqDataFin} ->
            %% TODO: websocket upgrade will be done differently
            {501, [], <<"Not Implemented">>}
    end;
handle(_Req, Args) ->
    ignore.

%%
%% TODO, use postprocess to let the dispatcher help with rendering error 
%% responses. This should not be done in the decision core.
%%
postprocess({_C, Rd}, Response, _Args) ->
    Response.
            

% @doc Handle event
%
handle_event(_Name, _EventArgs, _) -> ok.

%%
%% Helpers
%%

dispatch(Req, Dispatcher, DispatchArgs) ->
    case Dispatcher:dispatch(Req, DispatchArgs) of
        {{no_dispatch_match, Host, PathSpec}, ReqData} ->
            {undefined, ReqData};
        {{ControllerMod, ControllerOpts, 
          HostRemainder, Port, PathRemainder, PathBindings, AppRoot, StringPath}, ReqData} ->
            %% TODO -- Clean up this mess. fill the rest of the request data.
            ReqData1 = ReqData#machine_reqdata{req=Req},
            Controller = init_controller(ControllerMod, ControllerOpts),
            {Controller, ReqData1}
    end.

init_controller(ControllerMod, ControllerOpts) ->
    {ok, ControllerState} = elli_machine_controller:init(ControllerMod, ControllerOpts),
    {ControllerMod, ControllerState}.

dispatcher(Args) ->
    proplists:get_value(dispatcher, Args).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

config() ->
    MachineConfig = [
        {dispatcher, {elli_machine_dispatcher, [
            {dispatch_list, [
                {[<<"hello">>, '*'], example_controller, []}
            ]}
        ]}}
    ], 

    MiddlewareConfig = [
        {mods, [
            {elli_machine, MachineConfig}
        ]}
    ],

    [{callback, elli_middleware},
     {callback_args, MiddlewareConfig}, {port, 8000}].

not_found_test() ->
    Config = config(),
    ?assertEqual({404, [], <<"Not Found">>},
                 elli_test:call('GET', <<"/not_found">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),
    ?assertEqual({404, [], <<"Not Found">>}, elli_test:call('GET', <<"/not_found">>, [], <<>>, Config)),
    ok.

hello_world_test() ->
    Config = config(),
    ?assertEqual({200, [{<<"Content-Encoding">>, <<"identity">>},
                        {<<"Content-Type">>, <<"text/html">>}], <<"Hello, new world">>},
                 elli_test:call('GET', <<"/hello">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),

    ?assertEqual({200, [{<<"Content-Encoding">>, <<"identity">>},
                        {<<"Content-Type">>, <<"text/html">>}], <<"Hello, new world">>},
                 elli_test:call('GET', <<"/hello">>, [], <<>>, Config)),

    ok.

head_test() ->
    %% Note: elli removes the body.
    Config = config(),
    ?assertEqual({200, [{<<"Content-Encoding">>, <<"identity">>},
                        {<<"Content-Type">>, <<"text/html">>}], <<"Hello, new world">>},
                 elli_test:call('HEAD', <<"/hello">>, 
                    [{<<"Host">>, <<"example.com">>}], <<>>, Config)),
    ok.

four_o_five_test() ->
    Config = config(),

    ?assertEqual({405, [{<<"Allow">>,"GET, HEAD"}], <<>>},
                 elli_test:call('POST', <<"/hello">>, 
                    [{<<"Host">>, <<"example.com">>}], <<"test=123">>, Config)),

    ok.


-endif. %% TEST
    

