
-module(example_controller).

-export([init/1, ping/2, to_html/2]).

init(_Args) -> 
	{ok, undefined}.

ping(ReqData, Context) ->
	{pong, ReqData, Context}.

to_html(ReqData, Context) -> 
	{<<"Hello, new world">>, ReqData, Context}.

% %% Alternative controller implementation

% %%
% call(ping, ReqData, Context) ->
% 	ping(ReqData, Context);

% %% 
% call(to_html, ReqData, Context) ->
% 	to_html(ReqData, Context);
% 	{<<"Hello, new world">>, ReqData, Context};

% %%
% call(F, ReqData, Context) ->
% 	%% Explicit delegation of default values can go here. 
% 	%% Cleaner than import tricks IMHO.
% 	%%html_controller_defaults:call(F, ReqData, Context).

