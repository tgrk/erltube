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
      {timeout, 100, {"Test foobar", fun test_foobar/0}}
     ]
    }.

%% =============================================================================
test_foobar() ->
    ?assert(false).
