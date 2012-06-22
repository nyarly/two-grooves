%% @author Judson Lester <nyarly@gmail.com>
%% @copyright 2012 Judson Lester.

%% @doc Callbacks for the two_grooves application.

-module(two_grooves_app).
-author('Judson Lester <nyarly@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for two_grooves.
start(_Type, _StartArgs) ->
    two_grooves_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for two_grooves.
stop(_State) ->
    ok.
