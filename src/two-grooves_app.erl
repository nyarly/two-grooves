%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the two-grooves application.

-module(two-grooves_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for two-grooves.
start(_Type, _StartArgs) ->
    two-grooves_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for two-grooves.
stop(_State) ->
    ok.
