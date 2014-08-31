
-module(test_auth_controller).

%%
%%
%%

-export([init/1, 
	ping/2, 
	
	is_authorized/2,

	render_error/4, 
	handle_event/3]).

init(Arg) ->
    {ok, Arg}.

ping(ReqData, Context) ->
    {pong, ReqData, Context}.

is_authorized(ReqData, {halt, Code}=Context) ->
    {{halt, 200}, hx:set_resp_body(<<"halt">>, ReqData), Context};
is_authorized(ReqData, {realm, Realm}=Context) ->
    {Realm, ReqData, Context}.

%% 
%%
render_error(Code, _Error, ReqData, Context) ->
	{<<"Error">>, ReqData, Context}.

%%
%%
handle_event(_, _, _) ->
	ok.
