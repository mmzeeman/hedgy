%%
%%
%%

-module(example_controller).

-export([init/1, ping/2, to_html/2, render_error/4, handle_event/3]).

init(_Args) -> 
	{ok, undefined}.

ping(Exchange, Context) ->
	{pong, Exchange, Context}.

to_html(Exchange, Context) -> 
	{<<"Hello, new world">>, Exchange, Context}.

%%
%% 
%%

render_error(Code, _Error, Exchange, Context) ->
	{<<"Error">>, Exchange, Context}.

handle_event(_Name, _Args, _Context) ->
	%% io:fwrite(standard_error, "~p: ~p~n", [Name, Args]),
	ok.
