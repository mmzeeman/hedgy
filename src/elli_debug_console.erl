%%
%% A simple debug middleware which dumps error messages on the console.
%%

-module(elli_debug_console).
-export([handle/2, handle_event/3]).

%%
%% ELLI EVENT CALLBACKS
%%

handle(_,_) -> ignore.


%% elli_startup is sent when Elli is starting up. If you are
%% implementing a middleware, you can use it to spawn processes,
%% create ETS tables or start supervised processes in a supervisor
%% tree.
handle_event(elli_startup, [], _) -> ok;

%% request_complete fires *after* Elli has sent the response to the
%% client. Timings contains timestamps of events like when the
%% connection was accepted, when request parsing finished, when the
%% user callback returns, etc. This allows you to collect performance
%% statistics for monitoring your app.
handle_event(request_complete, [_Request,
                                _ResponseCode, _ResponseHeaders, _ResponseBody,
                                _Timings], _) -> ok;

%% request_throw, request_error and request_exit events are sent if
%% the user callback code throws an exception, has an error or
%% exits. After triggering this event, a generated response is sent to
%% the user.
handle_event(request_throw, [Request, Exception, Stacktrace], _) -> 
	io:fwrite(standard_error, "request_throw: ~p~nRequest ----: ~p~nStacktrace ----: ~p~n", 
		[Exception, Request, Stacktrace]),
	ok;
handle_event(request_error, [Request, Exception, Stacktrace], _) -> 
	io:fwrite(standard_error, "request_error: ~p~nRequest ----: ~p~nStacktrace ----: ~p~n", 
		[Exception, Request, Stacktrace]),
	ok;
handle_event(request_exit, [Request, Exception, Stacktrace], _) -> 
	io:fwrite(standard_exit, "request_exit: ~p~nRequest ----: ~p~nStacktrace ----: ~p~n", 
		[Exception, Request, Stacktrace]),
	ok;


%% chunk_complete fires when a chunked response is completely
%% sent. It's identical to the request_complete event, except instead
%% of the response body you get the atom "client" or "server"
%% depending on who closed the connection.
handle_event(chunk_complete, [_Request,
                              _ResponseCode, _ResponseHeaders, _ClosingEnd,
                              _Timings], _) -> ok;

%% request_closed is sent if the client closes the connection when
%% Elli is waiting for the next request on a keep alive connection.
handle_event(request_closed, [], _) -> ok;

%% request_timeout is sent if the client times out when
%% Elli is waiting for the request.
handle_event(request_timeout, [], _) -> ok;

%% request_parse_error fires if the request is invalid and cannot be
%% parsed by erlang:decode_packet/3 or it contains a path Elli cannot
%% parse or does not support.
handle_event(request_parse_error, [_], _) -> ok;

%% client_closed can be sent from multiple parts of the request
%% handling. It's sent when the client closes the connection or if for
%% any reason the socket is closed unexpectedly. The "Where" atom
%% tells you in which part of the request processing the closed socket
%% was detected: receiving_headers, receiving_body, before_response
handle_event(client_closed, [_Where], _) -> ok;

%% client_timeout can as with client_closed be sent from multiple
%% parts of the request handling. If Elli tries to receive data from
%% the client socket and does not receive anything within a timeout,
%% this event fires and the socket is closed.
handle_event(client_timeout, [_Where], _) -> ok;

%% bad_request is sent when Elli detects a request is not well
%% formatted or does not conform to the configured limits. Currently
%% the Reason variable can be any of the following: {too_many_headers,
%% Headers}, {body_size, ContentLength}
handle_event(bad_request, [_Reason], _) -> ok;

%% file_error is sent when the user wants to return a file as a
%% response, but for some reason it cannot be opened.
handle_event(file_error, [_ErrorReason], _) -> ok.
