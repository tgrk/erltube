%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Youtube v3 API
%%% @end
%%% Created : 4 Mar 2014 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erltube).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([request_token/3,
         request_token/4,
         authorize_token/4,
         refresh_token/3,
         revoke_token/1,
         scope/1,

         get_channel/2,
         update_channel/3,
         get_playlist/3,

         start/0,
         stop/0
        ]).

-ifdef(EUNIT).
-export([normalize/1]).
-endif.

-define(DEPS, [crypto, asn1, public_key, ssl, inets, jiffy]).
-define(URL_API, "https://www.googleapis.com/youtube/v3/").
-define(URL_OAUTH, "https://accounts.google.com/o/oauth2/").

%%%============================================================================
%%% API
%%%============================================================================
request_token(ClientKey, RedirectUri, Redirect) ->
    request_token(ClientKey, RedirectUri, scope(readonly), Redirect).

request_token(ClientKey, RedirectUri, Scope, Redirect) ->
    Params = [{client_id, ClientKey},
              {redirect_uri, RedirectUri},
              {response_type, code},
              {scope, Scope}],
    BaseUrl = get_url(request_token),
    case Redirect of
        true  -> http_request(get, BaseUrl, Params);
        false -> create_url(BaseUrl, Params)
    end.

authorize_token(ClientKey, ClientSecret, RedirectUri, Code) ->
    Params = [{code, Code},
              {client_id, ClientKey},
              {client_secret, ClientSecret},
              {redirect_uri, RedirectUri},
              {grant_type, authorization_code}],
    http_request(post, get_url(authorize_token), Params).

refresh_token(ClientKey, ClientSecret, RefreshToken) ->
    Params = [{client_id, ClientKey},
              {client_secret, ClientSecret},
              {refresh_token, RefreshToken},
              {grant_type, refresh_token}],
    http_request(post, get_url(authorize_token), Params).

revoke_token(Token) ->
    http_request(get, get_url(revoke_token), [{token, Token}]).

scope(readonly) ->
    "https://www.googleapis.com/auth/youtube.readonly";
scope(audit) ->
    "https://www.googleapis.com/auth/youtubepartner-channel-audit".

get_channel(AccessToken, Params) ->
    call_api(get, AccessToken, channel_list, Params, []).

update_channel(AccessToken, Params, Body) ->
    call_api(put, AccessToken, channel_update, Params, Body).

get_playlist(AccessToken, PlaylistId, Params) ->
    Params1 = [{playlistId, PlaylistId}] ++ Params,
    call_api(get, AccessToken, playlist_list, Params1, []).

start() ->
    [application:start(A) || A <- ?DEPS ++ [erltube]],
    ok.

stop() ->
    [application:stop(A) || A <- ?DEPS ++ [erltube]],
    ok.


%%%============================================================================
%%% Internal functionality
%%%============================================================================
call_api(Method, AccessToken, Resource, Params, Body) ->
    Headers = [{"Authorization", "Bearer " ++ AccessToken}],
    case check_params(Resource, Params) of
        ok ->
            Url = get_url(Resource),

            %% ?debugFmt("erltube", []),
            %% ?debugFmt("-------------------------------------------------", []),
            %% ?debugFmt("url=~p", [Url]),
            %% ?debugFmt("params=~p",[Params]),
            %% ?debugFmt("body=~p",[Body]),
            %% ?debugFmt("-------------------------------------------------", []),
            case config(verbose, true)of
                true  -> ?debugFmt("erltube: url=~p, params=~p, body=~p~n",
                                   [Url, Params, Body]);
                false -> skip
            end,

            case http_request(Method, Url, Headers, Params, Body) of
                {200, ResponseHeaders, ResponseBody} ->
                    {ok, ResponseHeaders, parse_response(ResponseBody, json)};
                {Code, ResponseHeaders, ResponseBody} ->
                    {error, {Code, ResponseHeaders,
                             parse_response(ResponseBody, json)}}
            end;
        Error ->
            Error
    end.

check_params(Resource, Params) ->
    Rules = get_resource_params(Resource),
    case check_required_params(Resource, Rules, Params) of
        ok ->
            case check_one_or_more_params(Resource, Rules, Params) of
                ok ->
                    check_params_types(Resource, Params);
                Error1 ->
                    Error1
            end;
        Error ->
            Error
    end.

check_params_types(Resource, Params) ->
    WrongTypeParams = lists:filtermap(
                        fun(Param) ->
                                case validate_params(Resource, Param) of
                                    true  -> false;
                                    false -> {true, Param}
                                end
                        end, Params),
    case length(WrongTypeParams) =:= 0 of
        true  -> ok;
        false -> {error, {invalid_param_types, Resource, WrongTypeParams}}
    end.

check_required_params(Resource, Rules, Params) ->
    Required = proplists:get_value(required, Rules, []),
    MissingParams = lists:filtermap(
                      fun(K) ->
                              case proplists:is_defined(K, Params) of
                                  true  -> false;
                                  false -> {true, K}
                              end
                      end, Required),
    case length(MissingParams) =:= 0 of
        true  -> ok;
        false -> {error, {missing_required_params, Resource, MissingParams}}
    end.

check_one_or_more_params(Resource, Rules, Params) ->
    OneOrMore = proplists:get_value(one_or_more, Rules, []),
    MissingParams = lists:filtermap(
                      fun(K) ->
                              case proplists:is_defined(K, Params) of
                                  true  -> false;
                                  false -> {true, K}
                              end
                      end, OneOrMore),
    case length(MissingParams) =/= length(OneOrMore) of
        true  -> ok;
        false -> {error, {missing_optional_params, Resource, MissingParams}}
    end.

get_resource_params(channel_list) ->
    [{required,    [part]},
     {one_or_more, [categoryId, forUsername, managedByMe, mine, mySubscribers]}
    ];
get_resource_params(channel_update) ->
    [{required,    [part]}];
get_resource_params(playlist_list) ->
    [{required,   []},
    {one_or_more, []}].

validate_params(channel_list, {part, Value}) ->
    lists:member(Value, [snippet, auditDetails,
                         brandingSettings, contentDetails,
                         invideoPromotion, statistics,
                         status, topicDetails]);
validate_params(channel_list, {id, Value}) when is_list(Value) ->
    true;
validate_params(channel_list, {categoryId, Value}) when is_list(Value) ->
    true;
validate_params(channel_list, {forUsername, Value}) when is_list(Value) ->
    true;
validate_params(channel_list, {managedByMe, Value}) when is_boolean(Value) ->
    true;
validate_params(channel_list, {mine, Value}) when is_boolean(Value) ->
    true;
validate_params(channel_list, {mySubscribers, Value}) when is_boolean(Value) ->
    true;
validate_params(channel_list, {maxResults, Value}) when is_integer(Value) ->
    true;
validate_params(channel_list, {onBehalfOfContentOwner, Value})
  when is_list(Value) ->
    true;
validate_params(channel_list, {pageToken, Value}) when is_list(Value) ->
    true;
validate_params(channel_update, {part, Value}) ->
    lists:member(Value, [id, brandingSettings, invideoPromotion]);
validate_params(channel_update, {onBehalfOfContentOwner, Value})
  when is_list(Value) ->
    true;
validate_params(_Resource, _Params) ->
    false.

parse_response(Body, params) ->
    parse_params(Body);
parse_response(Body, json) ->
    case config(use_maps) =:= true of
        true  -> jiffy:decode(to_binary(Body), [return_maps]);
        false -> jiffy:decode(to_binary(Body))
    end.

normalize(JsonStruct) ->
    normalize(JsonStruct, []).

normalize([], Acc) ->
    lists:reverse(Acc);
normalize([{PL}], Acc) ->
    normalize(PL, Acc);
normalize({[H | T]}, Acc) ->
    normalize(T, lists:reverse(normalize_item(H, [])) ++ Acc);
normalize([H | T], Acc) ->
    normalize(T, lists:reverse(normalize_item(H, [])) ++ Acc).

normalize_item({K,{V}}, Acc) ->
    [{K, normalize(V, Acc)} | Acc];
normalize_item({K,V}, Acc) when is_list(V) ->
    [{K, normalize(V, Acc)} | Acc];
normalize_item({K,V}, Acc) ->
    [{K, V} | Acc].

http_request(get, BaseUrl, Params) ->
    Url = create_url(BaseUrl, Params),
    {ok, {{_, Status, _}, ResponseHeaders, Body}} =
        httpc:request(get, {Url, []}, [{timeout, config(timeout)}], []),
    {Status, ResponseHeaders, Body};
http_request(post, Url, Params) ->
    Args = create_url_params(Params),
    {ok, {{_, Status, _}, ResponseHeaders, ResponseBody}} =
        httpc:request(post,
                      {Url, [], "application/x-www-form-urlencoded", Args},
                      [{timeout, config(timeout)}], []),
    {Status, ResponseHeaders, ResponseBody}.

http_request(Method, BaseUrl, Headers, Params, Body) ->
    Url = create_url(BaseUrl, Params),
    {ok, {{_, Status, _}, ResponseHeaders, ResponseBody}} =
        httpc:request(Method, {Url, Headers},
                      [{timeout, config(timeout)}], Body),
    {Status, ResponseHeaders, ResponseBody}.

create_url(BaseUrl, Params) ->
    BaseUrl ++ "?" ++ create_url_params(Params).

create_url_params(Params) ->
    lists:flatmap(fun ({K,V}) -> to_list(K) ++ "=" ++ to_list(V) ++ "&" end,
                  Params).

parse_params(Input) ->
    lists:map(fun parse_param/1, string:tokens(Input, "&")).

parse_param(Input) ->
    [Key, Value] = string:tokens(Input, "="),
    {list_to_atom(Key), Value}.

%%FIXME: remove duplication
get_url(request_token) ->
    ?URL_OAUTH ++ "auth";
get_url(authorize_token) ->
    ?URL_OAUTH ++ "token";
get_url(revoke_token) ->
    ?URL_OAUTH ++ "revoke";
get_url(channel_list) ->
    ?URL_API ++ "channels";
get_url(channel_update) ->
    ?URL_API ++ "channels";
get_url(playlist_list) ->
    ?URL_API ++ "playlistItems".

to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_list(Value) ->
    Value.

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) ->
    Value.

config(Key) ->
    {ok, Val} = application:get_env(erltube, Key),
    Val.

config(Key, Default) ->
    application:get_env(erltube, Key, Default).
