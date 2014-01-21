%% Original webmachine_controller.erl by
%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies

%% Adapted to work with elli by 
%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>

%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(elli_machine_controller).

-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Marc Worrell <marc@worrell.nl>').
-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').

-export([
    init/2,
    call/2,

    handle_event/3,
    render_error/4
]).

-include("elli_machine.hrl").

        
% @doc Intitialize controller Mod.
-spec init(module(), any()) -> {ok, {module(), any()}}.  
init(Mod, ModArgs) ->
    {ok, _State} = Mod:init(ModArgs).

%%
%%
call(Fun, FlowState) when is_atom(Fun) ->
    case erlang:function_exported(Mod, Fun, 2) of
        true ->
            controller_call(Fun, FlowState);
        false ->
            {use_default(Fun, Mod), Flow}
    end.


%%
%%
handle_event({Mod, State}, Name, EventArgs) ->
    Mod:handle_event(Name, EventArgs, State).

render_error({Mod, State}=Controller, Code, Reason, ReqData) ->
    case erlang:function_exported(Mod, render_error, 4) of
        true ->
            {ErrorHtml, ReqData1, State1} = Mod:render_error(Code, Reason, ReqData, State),
            {ErrorHtml, {Mod, State1}, ReqData1};
        false ->
            {ErrorHtml, ReqData1} = elli_machine_error_handler:render_error(Code, Reason, ReqData),
            {ErrorHtml, Controller, ReqData1}
    end.

%%
%% Helpers
%%

controller_call(F, #machine_flow_state{exchange=Exc, controller_mod=Mod, controller_state=State}=Flow) ->
    {Res, Exchange1, State1} = Mod:F(Exc, State),
    {Res, Flow#machine_flow_state{exchange=Exchange, controller_state=State1}}.


use_default(Fun, Mod) ->
    case default(Fun) of
        no_default ->
            {error, {no_default, Fun, Mod}};
        Default ->
            Default
    end.

default(ping) ->
    no_default;
default(service_available) ->
    true;
default(resource_exists) ->
    true;
default(auth_required) ->
    true;
default(is_authorized) ->
    true;
default(forbidden) ->
    false;
default(upgrades_provided) ->
    [];
default(allow_missing_post) ->
    false;
default(malformed_request) ->
    false;
default(uri_too_long) ->
    false;
default(known_content_type) ->
    true;
default(valid_content_headers) ->
    true;
default(valid_entity_length) ->
    true;
default(options) ->
    [];
default(allowed_methods) ->
    ['GET', 'HEAD'];
default(known_methods) ->
    ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT', 'OPTIONS'];
default(content_types_provided) ->
    [{<<"text/html">>, to_html}];
default(content_types_accepted) ->
    [];
default(delete_resource) ->
    false;
default(delete_completed) ->
    true;
default(post_is_create) ->
    false;
default(create_path) ->
    undefined;
default(base_uri) ->
    undefined;
default(process_post) ->
    no_default;
default(language_available) ->
    true;
default(charsets_provided) ->
    no_charset; % this atom causes charset-negotation to short-circuit
% the default setting is needed for non-charset responses such as image/png
%    an example of how one might do actual negotiation
%    [{"iso-8859-1", fun(X) -> X end}, {"utf-8", make_utf8}];
default(encodings_provided) ->
    [{<<"identity">>, fun(X) -> X end}];
% this is handy for auto-gzip of GET-only resources:
%    [{"identity", fun(X) -> X end}, {"gzip", fun(X) -> zlib:gzip(X) end}];
default(variances) ->
    [];
default(is_conflict) ->
    false;
default(multiple_choices) ->
    false;
default(previously_existed) ->
    false;
default(moved_permanently) ->
    false;
default(moved_temporarily) ->
    false;
default(last_modified) ->
    undefined;
default(expires) ->
    undefined;
default(generate_etag) ->
    undefined;
default(finish_request) ->
    true;
default(validate_content_md5) ->
    not_validated;
default(_) ->
    no_default.


