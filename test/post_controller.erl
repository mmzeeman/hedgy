%%
%%
%%

-module(post_controller).

-export([init/1, 
	ping/2, 
	allowed_methods/2, 
	to_html/2,
	process_post/2,
	render_error/4, 
	handle_event/3]).

init(_Args) -> 
	{ok, undefined}.

ping(ReqData, Context) ->
	{pong, ReqData, Context}.

allowed_methods(ReqData, Context) ->
	{['POST'], ReqData, Context}.

process_post(ReqData, Context) ->
	{true, ReqData, Context}.

to_html(ReqData, Context) -> 
	{<<"Hello, new world">>, ReqData, Context}.

%%
%% 
%%

render_error(Code, Error, ReqData, Context) ->
	io:fwrite(standard_error, "error: ~p ~p~n", [Code, Error]),
	{<<"Error">>, ReqData, Context}.

%%
%%
handle_event(core_decision=Name, [DecisionID, _ReqData], _Context) ->
	io:fwrite(standard_error, "~p: ~p~n", [Name, DecisionID]),
	ok;
handle_event(core_error=Name, Args, _Context) ->
	io:fwrite(standard_error, "~p: ~p~n", [Name, Args]),
	ok;
handle_event(_, _, _) ->
	ok.
