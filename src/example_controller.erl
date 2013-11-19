%%
%%
%%

-module(example_controller).

-export([init/1, ping/2, to_html/2]).

init(_Args) -> 
	{ok, undefined}.

ping(ReqData, Context) ->
	{pong, ReqData, Context}.

to_html(ReqData, Context) -> 
	{<<"Hello, new world">>, ReqData, Context}.

