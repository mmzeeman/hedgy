-module(test_ws_controller).


%%
%%
%%

-export([init/1, 
    ping/2, 
    
    upgrades_provided/2,

    to_html/2,

    render_error/4, 
    handle_event/3]).

-export([websocket_start/2]).

init(_Args) -> 
    {ok, undefined}.

ping(ReqData, Context) ->
    {pong, ReqData, Context}.

%% @doc Possible connection upgrades
upgrades_provided(ReqData, Context) ->
    {[{<<"WebSocket">>, websocket_start}], ReqData, Context}.

websocket_start(ReqData, Context) ->
    undefined.

to_html(ReqData, Context) ->
    {<<"No websocket headers">>, ReqData, Context}.

%% 
%%
render_error(Code, Error, ReqData, Context) ->
    {<<"Error">>, ReqData, Context}.

%%
%%
handle_event(A, B, C) ->
    ok.
