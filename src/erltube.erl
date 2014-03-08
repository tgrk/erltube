%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Youtube v3 API
%%% @end
%%% Created : 4 Mar 2014 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erltube).

%% API
-export([request_token/3,
         request_token/4,
         authorize_token/4,
         refresh_token/3,
         revoke_token/1,
         scope/1,

         get_channel/2,
         get_playlist/3,
         is_valid_param/2,

         start/0,
         stop/0
        ]).

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
    call_api(AccessToken, channel, Params).

get_playlist(AccessToken, PlaylistId, Params) ->
    Params1 = [{playlistId, PlaylistId}] ++ Params,
    call_api(AccessToken, playlist, Params1).

is_valid_param(Resource, Params) ->
    not lists:member(false, [validate_params(Resource, F) || F <- Params]).

start() ->
    [application:start(A) || A <- ?DEPS ++ [erltube]],
    ok.

stop() ->
    [application:stop(A) || A <- ?DEPS ++ [erltube]],
    ok.


%%%============================================================================
%%% Internal functionality
%%%============================================================================
call_api(AccessToken, Resource, Params) ->
    Headers = [{"Authorization", "Bearer " ++ AccessToken}],
    case is_valid_param(Resource, Params) of
        true ->
            case http_request(get, get_url(Resource), Headers, Params) of
                {200, Headers, Response} ->
                    {ok, Headers, parse_response(Response, json)};
                {400, Headers, _} ->
                    {error, {bad_request, Headers}};
                {403, Headers, _} ->
                    {error, {unauthorized, Headers}};
                {_Other, Headers, _Reason} ->
                    {error, Headers}
            end;
        false ->
            {error, {invalid_params, Params}}
    end.

%%TODO: handle required, at least one param and optional params
validate_params(channel, {part, Value}) ->
    lists:member(Value, [snippet, auditDetails,
                         brandingSettings, contentDetails,
                         invideoPromotion, statistics,
                         status, topicDetails]);
validate_params(channel, {id, Value}) when is_list(Value) ->
    true;
validate_params(channel, {categoryId, Value}) when is_list(Value) ->
    true;
validate_params(channel, {forUsername, Value}) when is_list(Value) ->
    true;
validate_params(channel, {managedByMe, Value}) when is_boolean(Value) ->
    true;
validate_params(channel, {mine, Value}) when is_boolean(Value) ->
    true;
validate_params(channel, {mySubscribers, Value}) when is_boolean(Value) ->
    true;
validate_params(channel, {maxResults, Value}) when is_integer(Value) ->
    true;
validate_params(channel, {onBehalfOfContentOwner, Value}) when is_list(Value) ->
    true;
validate_params(channel, {pageToken, Value}) when is_list(Value) ->
    true;
validate_params(_Resource, _Params) ->
    false.

parse_response(Response, params) ->
    parse_params(Response);
parse_response(Response, json) ->
    jiffy:decode(to_binary(Response)).

http_request(get, BaseUrl, Params) ->
    Url = create_url(BaseUrl, Params),
    {ok, {{_, Status, _}, Headers, Response}} =
        httpc:request(get, {Url, []}, [{timeout, infinity}], []),
    {Status, Headers, Response};
http_request(post, Url, Params) ->
    Args = create_url_params(Params),
    {ok, {{_, Status, _}, Headers, Response}} =
        httpc:request(post,
                      {Url, [], "application/x-www-form-urlencoded", Args},
                      [{timeout, infinity}], []),
    {Status, Headers, Response}.

http_request(get, BaseUrl, Headers, Params) ->
    Url = create_url(BaseUrl, Params),
    {ok, {{_, Status, _}, Headers, Response}} =
        httpc:request(get, {Url, Headers}, [{timeout, infinity}], []),
    {Status, Headers, Response}.

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

get_url(request_token) ->
    ?URL_OAUTH ++ "auth";
get_url(authorize_token) ->
    ?URL_OAUTH ++ "token";
get_url(revoke_token) ->
    ?URL_OAUTH ++ "revoke";
get_url(channel) ->
    ?URL_API ++ "channels";
get_url(playlist) ->
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
