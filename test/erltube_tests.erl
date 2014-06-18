%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Youtube API v3
%%% @end
%%% Created : 4 Mar 2014 by Martin Wiso <tajgur@gmail.com>
%%%----------------------------------------------------------------------------
-module(erltube_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


%% =============================================================================
erltube_test_() ->
    {setup,
     fun() -> erltube:start() end,
     fun(_) -> erltube:stop() end,
     [
      {timeout, 100, {"Custom oAuth test", fun test_oauth/0}},
      {timeout, 100, {"Get channel params", fun test_get_channel_params/0}}
     ]
    }.

%% =============================================================================
test_oauth() ->
    Keys = read_api_keys(),
    ClientKey = proplists:get_value(client_id, Keys),
    ClientSecret = proplists:get_value(client_secret, Keys),
    RedirectUri = proplists:get_value(redirect_uri, Keys),
    Code = proplists:get_value(code, Keys),

    case erltube:authorize_token(ClientKey, ClientSecret,
                                 RedirectUri, Code) of
        {200, _, Body} ->
            {ResultPL} = jiffy:decode(Body),
            AccessToken = proplists:get_value(<<"access_token">>, ResultPL),
            NewKeys = get_keys_struct(ClientKey, ClientSecret, RedirectUri,
                                      Code, AccessToken),
            write_api_keys(NewKeys),
            ?assert(true);
        {400, _, ErrorResult} ->
            case jiffy:decode(ErrorResult) of
                {[{<<"error">>,<<"invalid_grant">>} | _]} ->
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
read_api_keys() ->
    case file:consult("../api.txt") of
        {ok,[Keys]} -> Keys;
        _ -> throw("Unable to read credentials from api.txt file!")
    end.

write_api_keys(PL) ->
    file:write_file("../api.txt", io_lib:format("~p.", [PL])).

get_keys_struct(ClientKey, ClientSecret, RedirectUri, Code, AccessToken) ->
    [
     {client_id, ensure_list(ClientKey)},
     {client_secret, ensure_list(ClientSecret)},
     {redirect_uri, ensure_list(RedirectUri)},
     {code, ensure_list(Code)},
     {access_token, ensure_list(AccessToken)}
    ].

request_token(Keys) ->
    ClientKey = proplists:get_value(client_id, Keys),
    RedirectUri = proplists:get_value(redirect_uri, Keys),
    erltube:request_token(ClientKey, RedirectUri, false).

ensure_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
ensure_list(Value) ->
    Value.
