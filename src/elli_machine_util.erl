%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%%
%% @doc Elli Machine Utils
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

%% Implementations taken from webmachine_util.erl and made binary versions.

-module(elli_machine_util).

-include_lib("elli/include/elli_util.hrl").

-export([
    host/1,

    select_media_type/2,
    select_content_type/2,

    choose_charset/2,
    choose_encoding/2,

    split_quoted_strings/1,

    binary_join/2,
    remove_whitespace/1,

    rfc1123_date/1
]).

% @doc Return the hostname of the request, or undefined if no host can be found.
-spec host(Headers :: elli:headers()) -> undefined | binary().
host(undefined) ->
    undefined;
host(Headers) ->
    case host_headers(Headers, undefined, undefined, undefined) of
        [] -> undefined;
        [H|_] -> H
    end.

%% @doc Select the media type for this response.
select_media_type(Provided, AcceptHeader) ->
    ContentTypes = [normalize_provided1(Type) || {Type, _Fun} <- Provided],
    case choose_media_type(ContentTypes, AcceptHeader) of
        none -> none;
        ChosenMediaType -> 
            {ChosenMediaType, _Fun} = proplists:lookup(ChosenMediaType, Provided)
    end.

%% %doc Choose the content-type that will be used to accept an request.
select_content_type(Provided, ContentTypeHeader) ->
    % Return the content-type that will be used to accept the request.
    ContentTypes = [normalize_provided1(Type) || {Type, _Fun} <- Provided],
    ContentTypeDetail = media_type_to_detail(ContentTypeHeader),
    
    % Provided is a list of media types the controller can accept.
    %  each is either a string e.g. -- <<"application/x-www-form-urlencoded">>
    %   or a string and parameters e.g. -- {<<"x-www-form-urlencoded">>,[{level,1}]}
    % (the plain string case with no parameters is much more common)
    case choose_content_type(ContentTypeDetail, ContentTypes) of
        none -> none;
        MediaType ->
            {MediaType, _Fun} = proplists:lookup(MediaType, Provided)
    end.

%% @doc Choose media type
choose_media_type(Provided, AcceptHead) ->
    % Return the Content-Type we will serve for a request.
    % If there is no acceptable/available match, return the atom "none".
    % AcceptHead is the value of the request's Accept header

    % Provided is a list of media types the controller can provide.
    %  each is either a string e.g. -- <<"text/html">>
    %   or a string and parameters e.g. -- {<<"text/html">>,[{level,1}]}
    % (the plain string case with no parameters is much more common)
    Requested = accept_header_to_media_types(AcceptHead),
    %% Prov1 = normalize_provided(Provided),
    choose_media_type1(Provided, Requested).


%% Choose media type to accept an incoming request.
choose_content_type(_, []) ->
    none;
choose_content_type({MediaType, _Params}=ContentType, [ {MediaTypeHead, _ParamsHead}=O | T]) ->
    case media_type_match(MediaTypeHead, MediaType) of
        false -> choose_content_type(ContentType, T);
        true -> MediaTypeHead
    end.


% @doc Select a charset.
choose_charset(no_charset, _) ->
    no_charset;
choose_charset(CSets, AccCharHdr) ->
    do_choose(CSets, AccCharHdr, <<"ISO-8859-1">>).

% @doc Select an encoding.
choose_encoding(Encs, AccEncHdr) ->
    do_choose(Encs, AccEncHdr, <<"identity">>).

% @doc Join the binary strings in the list with the sep
-spec binary_join(list((binary())), integer() | binary()) -> binary().
binary_join(List, Sep) ->
    binary_join(List, Sep, <<>>).
binary_join([], _Sep, Acc) ->
    Acc;
binary_join([H|T], Sep, <<>>) ->
    binary_join(T, Sep, <<H/binary>>);
binary_join([H|T], Sep, Acc) when is_binary(Sep) ->
    binary_join(T, Sep, <<Acc/binary, Sep/binary, H/binary>>);
binary_join([H|T], Sep, Acc) when is_integer(Sep) ->
    binary_join(T, Sep, <<Acc/binary, Sep, H/binary>>).

% @doc Remove whitespace from Bin
-spec remove_whitespace(binary()) -> binary().
remove_whitespace(Bin) ->
    << <<C>> || <<C>> <= Bin, not (C =:= $\s orelse C=:=$\t orelse C=:= $\r orelse C =:= $\n) >>.

% @doc return utc Date in rfc1123 format.
-spec rfc1123_date(calendar:datetime()) -> binary().
rfc1123_date({{YYYY, MM, DD}=Date, {Hour, Min, Sec}}) ->
    DN = calendar:day_of_the_week(Date),
    << (day(DN))/binary, ", ", (pad(DD))/binary, " ", (month(MM))/binary, " ", (year(YYYY))/binary, " ", 
    (pad(Hour))/binary, ":", (pad(Min))/binary, ":", (pad(Sec))/binary, " GMT" >>.


%%
%% Helpers
%% 

% @doc Return a prioritized list with host header values.
host_headers([], Prio1, Prio2, Prio3) ->
    [V || V <- [Prio1, Prio2, Prio3], V =/= undefined];
host_headers([{<<"X-Forwarded-Host">>, ForwardedHost}|Rest], undefined, Prio2, Prio3) ->
    host_headers(Rest, ForwardedHost, Prio2, Prio3);
host_headers([{<<"X-Forwarded-Server">>, ForwardedServer}|Rest], Prio1, undefined, Prio3) ->
    host_headers(Rest, Prio1, ForwardedServer, Prio3);
host_headers([{<<"Host">>, Host}|Rest], Prio1, Prio2, undefined) ->
    host_headers(Rest, Prio1, Prio2, Host);
host_headers([_|Rest], Prio1, Prio2, Prio3) ->
    host_headers(Rest, Prio1, Prio2, Prio3).


choose_media_type1(_Provided,[]) ->
    none;
choose_media_type1(Provided, [H|T]) ->
    {_Pri,Type,Params} = H,
    case media_match({Type,Params}, Provided) of
        [] -> choose_media_type1(Provided,T);
        [{CT_T,CT_P}|_] -> format_content_type(CT_T,CT_P)
    end.

media_match(_, []) -> 
    [];
media_match({<<"*/*">>, []}, [H|_]) -> 
    [H];
media_match({Type, Params}, Provided) ->
    [{T1,P1} || {T1,P1} <- Provided, media_type_match(Type, T1), media_params_match(Params, P1)].


media_type_match(<<"*">>, _Prov) -> true;
media_type_match(<<"*/*">>, _Prov) -> true;
media_type_match(Req, Req) -> true;
media_type_match(Req, Prov) ->
    [R1 | R2] = binary:split(Req, <<"/">>, [global]),
    [P1, _P2] = binary:split(Prov, <<"/">>, [global]),
    case R2 of
        [<<"*">>] ->
            case R1 of
                P1 -> true;
                _ -> false
            end;
        _ -> false
    end.

media_params_match(Req, Prov) ->
    lists:sort(Req) =:= lists:sort(Prov).

prioritize_media(TyParam) ->
    {Type, Params} = TyParam,
    prioritize_media(Type,Params,[]).    
prioritize_media(Type,Params,Acc) ->
    case Params of
        [] ->
            {1, Type, Acc};
        _ ->
            [{Tok,Val}|Rest] = Params,
            case Tok of
                <<"q">> ->
                    {qval(Val), Type, Rest ++ Acc};
                _ ->
                    prioritize_media(Type,Rest,[{Tok,Val}|Acc])
            end
    end.

-spec qval(binary()) -> number().
qval(<<"1">>) -> 
    1;
qval(<<$., Val/binary>>) ->
    list_to_float([$0, $. | binary_to_list(Val)]);
qval(Val) ->
    list_to_float(binary_to_list(Val)).

media_type_to_detail(MType) ->
    [CType|Params] = binary:split(remove_whitespace(MType), <<$;>>, [global]),
    MParams = [list_to_tuple([KV || KV <- binary:split(X, <<$=>>), KV=/=<<>>]) 
        || X <- Params, X=/=<<>>],
    {CType, MParams}.                       

accept_header_to_media_types(HeadVal) ->
    % given the value of an accept header, produce an ordered list
    % based on the q-values.  Results are [{Type,Params}] with the
    % head of the list being the highest-priority requested type.
    try
        lists:reverse(lists:keysort(1,
         [prioritize_media(media_type_to_detail(MType)) || MType <- binary:split(HeadVal, <<",">>, [global])]))
    catch _:_ -> []
    end.

normalize_provided(Provided) ->
    [normalize_provided1(X) || X <- Provided].

normalize_provided1(Type) when is_binary(Type) -> 
    {Type, []};
normalize_provided1({Type,Params}) -> 
    {Type, Params}.

format_content_type(Type, []) -> 
    Type;
format_content_type(Type, [H|T]) ->
    format_content_type(<<Type/binary, "; ", H/binary>>, T).

do_choose(Choices, Header, Default) ->
    NoWsHeader = elli_machine_util:remove_whitespace(Header),
    Accepted = build_conneg_list(binary:split(NoWsHeader, <<",">>, [global])),
    DefaultPrio = [P || {P,C} <- Accepted, C =:= Default],
    StarPrio = [P || {P,C} <- Accepted, C =:= <<"*">>],
    DefaultOkay = case DefaultPrio of
        [] ->
            case StarPrio of
                [0.0] -> no;
                _ -> yes
            end;
        [0.0] -> no;
        _ -> yes
    end,
    AnyOkay = case StarPrio of
        [] -> no;
        [0.0] -> no;
        _ -> yes
    end,
    do_choose(Default, DefaultOkay, AnyOkay, Choices, Accepted).
do_choose(_Default, _DefaultOkay, _AnyOkay, [], []) ->
    none;
do_choose(_Default, _DefaultOkay, _AnyOkay, [], _Accepted) ->
    none;
do_choose(Default, DefaultOkay, AnyOkay, Choices, []) ->
    case AnyOkay of
        yes -> hd(Choices);
        no ->
            case DefaultOkay of
                yes ->
                    case lists:member(Default, Choices) of
                        true -> Default;
                        _ -> none
                    end;
                no -> none
            end
    end;
do_choose(Default, DefaultOkay, AnyOkay, Choices, [AccPair|AccRest]) ->
    {Prio, Acc} = AccPair,
    case Prio of
        0.0 ->
            do_choose(Default, DefaultOkay, AnyOkay,
                            lists:delete(Acc, Choices), AccRest);
        _ ->
            LAcc = elli_bstr:to_lower(Acc),
            LChoices = [elli_bstr:to_lower(X) || X <- Choices],
            % doing this a little more work than needed in
            % order to be easily insensitive but preserving
            case lists:member(LAcc, LChoices) of
                true -> 
                    hd([X || X <- Choices,
                             elli_bstr:to_lower(X) =:= LAcc]);
                false -> do_choose(Default, DefaultOkay, AnyOkay,
                                         Choices, AccRest)
            end
    end.

build_conneg_list(AccList) ->
    build_conneg_list(AccList, []).
build_conneg_list([], Result) -> lists:reverse(lists:sort(Result));
build_conneg_list([Acc|AccRest], Result) ->
    XPair = list_to_tuple([X || X <- binary:split(Acc, <<";">>, [global])]),
    Pair = case XPair of
        {Choice, <<Q, $=, PrioStr/binary>>} when Q=:=$Q; Q=:=$q ->
            case PrioStr of
                <<"0">> -> {0.0, Choice};
                <<"1">> -> {1.0, Choice};
                <<$., Val/binary>> ->
                    %% handle strange FeedBurner Accept
                    {list_to_float([$0, $. | binary_to_list(Val)]), Choice};
                _ -> 
                    {list_to_float(binary_to_list(PrioStr)), Choice}
            end;
        {Choice} ->
            {1.0, Choice}
    end,
    build_conneg_list(AccRest,[Pair|Result]).


quoted_string([$" | _Rest] = Str) ->
    Str;
quoted_string(Str) ->
    escape_quotes(Str, [$"]).                % Initialize Acc with opening quote

escape_quotes([], Acc) ->
    lists:reverse([$" | Acc]);               % Append final quote
escape_quotes([$\\, Char | Rest], Acc) ->
    escape_quotes(Rest, [Char, $\\ | Acc]);  % Any quoted char should be skipped
escape_quotes([$" | Rest], Acc) ->
    escape_quotes(Rest, [$", $\\ | Acc]);    % Unquoted quotes should be escaped
escape_quotes([Char | Rest], Acc) ->
    escape_quotes(Rest, [Char | Acc]).

split_quoted_strings(Str) ->
    split_quoted_strings(Str, []).

split_quoted_strings(<<>>, Acc) ->
    lists:reverse(Acc);
split_quoted_strings(<<$", Rest/binary>>, Acc) ->
    {Str, Cont} = unescape_quoted_string(Rest, <<>>),
    split_quoted_strings(Cont, [Str | Acc]);
split_quoted_strings(<<_, Rest/binary>>, Acc) ->
    split_quoted_strings(Rest, Acc).

unescape_quoted_string(<<>>, Acc) ->
    {Acc, <<>>};
unescape_quoted_string(<<$\\, Char, Rest/binary>>, Acc) -> % Any quoted char should be unquoted
    unescape_quoted_string(Rest, <<Acc/binary, Char>>);
unescape_quoted_string(<<$", Rest/binary>>, Acc) ->        % Quote indicates end of this string
    {Acc, Rest};
unescape_quoted_string(<<Char, Rest/binary>>, Acc) ->
    unescape_quoted_string(Rest, <<Acc/binary, Char>>).

day(1) -> <<"Mon">>;
day(2) -> <<"Tue">>;
day(3) -> <<"Wed">>;
day(4) -> <<"Thu">>;
day(5) -> <<"Fri">>;
day(6) -> <<"Sat">>;
day(7) -> <<"Sun">>.

pad(0) -> <<"00">>;
pad(1) -> <<"01">>;
pad(2) -> <<"02">>;
pad(3) -> <<"03">>;
pad(4) -> <<"04">>;
pad(5) -> <<"05">>;
pad(6) -> <<"06">>;
pad(7) -> <<"07">>;
pad(8) -> <<"08">>;
pad(9) -> <<"09">>;
pad(10) -> <<"10">>;
pad(11) -> <<"11">>;
pad(12) -> <<"12">>;
pad(13) -> <<"13">>;
pad(14) -> <<"14">>;
pad(15) -> <<"15">>;
pad(16) -> <<"16">>;
pad(17) -> <<"17">>;
pad(18) -> <<"18">>;
pad(19) -> <<"19">>;
pad(20) -> <<"20">>;
pad(21) -> <<"21">>;
pad(22) -> <<"22">>;
pad(23) -> <<"23">>;
pad(24) -> <<"24">>;
pad(25) -> <<"25">>;
pad(26) -> <<"26">>;
pad(27) -> <<"27">>;
pad(28) -> <<"28">>;
pad(29) -> <<"29">>;
pad(30) -> <<"30">>;
pad(31) -> <<"31">>.

month(1) -> <<"Jan">>;
month(2) -> <<"Feb">>;
month(3) -> <<"Mar">>;
month(4) -> <<"Apr">>;
month(5) -> <<"May">>;
month(6) -> <<"Jun">>;
month(7) -> <<"Jul">>;
month(8) -> <<"Aug">>;
month(9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

year(Year) ->
    list_to_binary(integer_to_list(Year)).

%%
%% Tests
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

host_test() ->
    ?assertEqual(undefined, host(undefined)),
    ?assertEqual(undefined, host([])),

    ?assertEqual(<<"example.com">>, host([{<<"Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Server">>, <<"example.com">>}])),

    ?assertEqual(<<"example.com">>, host([{<<"Content-Length">>, <<"100">>}, {<<"Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Host">>, <<"example.com">>}])),
    ?assertEqual(<<"example.com">>, host([{<<"X-Forwarded-Server">>, <<"example.com">>}])),

    % test prio handling
    ?assertEqual(<<"example.com">>, host([
        {<<"X-Forwarded-Server">>, <<"example.com">>}, 
        {<<"Host">>, <<"other.name.com">>} ])),

    ok.

format_content_type_test() ->
    ?assertEqual(<<"text/html">>, format_content_type(<<"text/html">>, [])),
    ?assertEqual(<<"text/html; foo">>, format_content_type(<<"text/html">>, [<<"foo">>])),
    ?assertEqual(<<"text/html; foo; bar">>, format_content_type(<<"text/html">>, [<<"foo">>, <<"bar">>])),
    ok.

binary_join_test() ->
    ?assertEqual(<<>>, binary_join([], $.)),
    ?assertEqual(<<>>, binary_join([], <<". ">>)),
    ?assertEqual(<<"een">>, binary_join([<<"een">>], <<", ">>)),
    ?assertEqual(<<"een, twee">>, binary_join([<<"een">>, <<"twee">>], <<", ">>)),
    ?assertEqual(<<"een, twee, drie">>, binary_join([<<"een">>, <<"twee">>, <<"drie">>], <<", ">>)),
    ?assertEqual(<<"een,twee,drie">>, binary_join([<<"een">>, <<"twee">>, <<"drie">>], $,)),
    ok.

remove_whitespace_test() ->
    ?assertEqual(<<"test">>, remove_whitespace(<<"t e s t">>)),
    ?assertEqual(<<"test">>, remove_whitespace(<<"t e\ts\rt\r\n">>)),
    ?assertEqual(<<"">>, remove_whitespace(<<"   ">>)),
    ?assertEqual(<<"">>, remove_whitespace(<<"">>)),
    ok.

media_type_to_detail_test() ->
    ?assertEqual({<<"application/xml">>, [{<<"q">>,<<"0.9">>}]}, 
        media_type_to_detail(<<"application/xml;q=0.9">>)),
    ?assertEqual({<<"application/xml">>, [{<<"q">>,<<"0.9">>}]}, 
        media_type_to_detail(<<"application/xml ; q = 0.9 ">>)),
    ?assertEqual({<<"application/xml">>, [{<<"q">>,<<"0.9">>}, {<<"x">>, <<"1">>}]}, 
        media_type_to_detail(<<"application/xml ;;; q = 0.9 ; x=1">>)),
    ?assertEqual({<<"application/xml">>, [{<<"foo">>}, {<<"bar">>}]}, 
        media_type_to_detail(<<"application/xml;foo;bar">>)),
    ?assertEqual({<<"application/xml">>, [{<<"foo">>}, {<<"bar">>}]}, 
        media_type_to_detail(<<"application/xml;foo;bar;;;;;">>)),
    ok.

qval_test() ->
    ?assertEqual(0.8, qval(<<"0.8">>)),
    ?assertEqual(0.8, qval(<<".8">>)),
    ?assertEqual(1, qval(<<"1">>)),
    ok.

accept_header_to_media_types_test() ->
    ?assertEqual([{1, <<"application/xhtml+xml">>, []},
                  {1, <<"text/html">>, []},
                  {0.9, <<"application/xml">>, []},
                  {0.8, <<"*/*">>, []}], 
        accept_header_to_media_types(<<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>)),
    ?assertEqual([{1, <<"text/html">>, []},
                  {0.9, <<"application/xml">>, []},
                  {0.2, <<"*/*">>, []}], 
        accept_header_to_media_types(<<"text/html,application/xml; q=.9, */*; q=0.2">>)),
    ?assertEqual([{1, <<"text/html">>, []}], 
        accept_header_to_media_types(<<"text/html">>)),
    ?assertEqual([], 
        accept_header_to_media_types(<<"text/html;q=wrong">>)),
    ok.

choose_media_type_test() ->
    Provided = {<<"text/html">>, []},
    ShouldMatch = [<<"*">>, <<"*/*">>, <<"text/*">>, <<"text/html">>],
    WantNone = [<<"foo">>, <<"text/xml">>, <<"application/*">>, <<"foo/bar/baz">>],
    [?assertEqual(<<"text/html">>, choose_media_type([Provided], I)) || I <- ShouldMatch ],
    [?assertEqual(none, choose_media_type([Provided], I)) || I <- WantNone ],
    ok.

choose_media_type_qval_test() ->
    Provided = [{<<"text/html">>, []}, {<<"image/jpeg">>, []}],
    HtmlMatch = [<<"image/jpeg;q=0.5, text/html">>,
                 <<"text/html, image/jpeg; q=0.5">>,
                 <<"text/*; q=0.8, image/*;q=0.7">>,
                 <<"text/*;q=.8, image/*;q=.7">>], %% strange FeedBurner format
    JpgMatch = [<<"image/*;q=1, text/html;q=0.9">>,
                <<"image/png, image/*;q=0.3">>],
    [?assertEqual(<<"text/html">>, choose_media_type(Provided, I)) || I <- HtmlMatch ],
    [?assertEqual(<<"image/jpeg">>, choose_media_type(Provided, I)) || I <- JpgMatch ],
    ok.

choose_encoding_test() ->
    ?assertEqual(<<"identity">>, choose_encoding([<<"identity">>], <<"gzip, deflate">>)),
    ok.


rfc1123_date_test() ->
    ?assertEqual(<<"Sun, 08 Nov 1970 08:00:00 GMT">>, rfc1123_date({{1970,11,8}, {8,0,0}})),
    ok.


select_content_type_test() ->
    ContentType = <<"application/x-www-form-urlencoded; charset=UTF-8">>,
    Provided = [{<<"*/*">>, accept_all}],

    %% 
    ?assertEqual({<<"*/*">>, accept_all}, 
        select_content_type([{<<"*/*">>, accept_all}], ContentType)),

    ok.

split_quoted_strings_test() ->
    ?assertEqual([<<"hi">>], split_quoted_strings(<<"\"hi\"">>)),
    ?assertEqual([<<"hi">>, <<"there">>], split_quoted_strings(<<"\"hi\" \"there\"">>)),
    ok.

-endif.
