%%
%%
%%

-module(test_post_controller).

-export([init/1, 
	ping/2, 
	allowed_methods/2, 

	process_request/2,

	render_error/4, 
	handle_event/3]).

init(_Args) -> 
	{ok, undefined}.

ping(ReqData, Context) ->
	{pong, ReqData, Context}.

allowed_methods(ReqData, Context) ->
	{['POST'], ReqData, Context}.

process_request(X, Context) ->
	X1 = hx:set_resp_body(<<"<html><head></head><body>thank-you</body></html>">>, X),
	{true, X1, Context}.

%% 
%%
render_error(Code, _Error, ReqData, Context) ->
	{<<"Error">>, ReqData, Context}.

%%
%%
handle_event(_, _, _) ->
	ok.
