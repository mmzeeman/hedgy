%%
%%
%%

-module(example_controller).

-export([init/1, ping/2, to_html/2, 
	expires/2, generate_etag/2, last_modified/2,
	render_error/4, handle_event/3]).

init(Props) -> 
	{ok, Props}.

ping(Exchange, Props) ->
	{pong, Exchange, Props}.

generate_etag(X, Props) ->
 	{proplists:get_value(etag, Props), X, Props}.

expires(X, Props) ->
	{proplists:get_value(expires, Props), X, Props}.

last_modified(X, Props) ->
	{proplists:get_value(last_modified, Props), X, Props}.

to_html(Exchange, Props) -> 
	{<<"Hello, new world">>, Exchange, Props}.

%%
%% 
%%

render_error(Code, _Error, Exchange, Context) ->
	{<<"Error">>, Exchange, Context}.

handle_event(_Name, _Args, _Context) ->
	%% io:fwrite(standard_error, "~p: ~p~n", [Name, Args]),
	ok.
