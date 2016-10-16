%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmail.com>
%%% @doc
%%% Erlang library for Youtube API v3
%%% @end
%%% Created : 4 Mar 2014 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erltube_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(STORAGE, erltube).

%% =============================================================================
erltube_test_() ->
    {setup,
     fun() ->
             {ok, _} = application:ensure_all_started(erltube),

             %% for storing oAuth credentials and reusing them in other thest
             _ = ets:new(?STORAGE, [set, named_table]),

             ok
     end,
     fun(_) -> application:stop(erltube) end,
     [
        {timeout, 100, {"Custom oAuth test",  fun test_oauth/0}}
      , {timeout, 100, {"Get channel params", fun test_get_channel_params/0}}
     ]
    }.

%% =============================================================================
test_oauth() ->
    Keys = read_api_keys(),
    ClientKey    = maps:get(client_id, Keys),
    ClientSecret = maps:get(client_secret, Keys),
    RedirectUri  = maps:get(redirect_uri, Keys),
    Code         = maps:get(code, Keys),

    case erltube:authorize_token(ClientKey, ClientSecret, RedirectUri, Code) of
        {ok, _Headers, Map} ->
            %% store oAuth credentials for later use
            AccessToken  = maps:get(<<"access_token">>, Map),
            RefreshToken = maps:get(<<"refresh_token">>, Map),
            ok = store_all(ClientKey, ClientSecret, RedirectUri,
                           Code, AccessToken, RefreshToken),
            ?assert(true);
        {error, {_ErrorCode, _ErrorHeaders, ErrorMap}} ->
            ?debugFmt("debug: error_map=~p", [ErrorMap]),
            case maps:get(<<"error">>, ErrorMap) of
                <<"invalid_grant">> ->
                    Url = request_token(Keys),
                    ?debugFmt("Open url ~p, paste code into api.txt file "
                              "and retry.", [Url]),
                    ?assert(false);
                Other ->
                    ?debugFmt("debug: other=~p", [Other])
            end
    end.

test_get_channel_params() ->
    ?assert(false).

%%%============================================================================
request_token(Keys) ->
    ClientKey   = maps:get(client_id, Keys),
    RedirectUri = maps:get(redirect_uri, Keys),
    erltube:request_token(ClientKey, RedirectUri, false).

read_api_keys() ->
    case file:consult("api.txt") of
        {ok,[Keys]} -> Keys;
        _ -> throw("Unable to read credentials from api.txt file!")
    end.

%% Helpers for storing credentials
store_all(ClientKey, ClientSecret, RedirectUri, Code, AccessToken, RefreshToken) ->
    true = store({client_id,     erltube:to_binary(ClientKey)}),
    true = store({client_secret, erltube:to_binary(ClientSecret)}),
    true = store({redirect_uri,  erltube:to_binary(RedirectUri)}),
    true = store({code,          erltube:to_binary(Code)}),
    true = store({access_token,  erltube:to_binary(AccessToken)}),
    true = store({refresh_token, erltube:to_binary(RefreshToken)}),
    ok.

store(Props) ->
    ets:insert(?STORAGE, Props).

lookup(Key) ->
    ets:lookup(?STORAGE, Key).
