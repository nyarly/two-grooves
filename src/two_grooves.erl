%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Two Grooves startup code
%%% @end
%%% Created :  Tue May 29 02:45:31 2012 by Judson Lester
-module(two_grooves).

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    two_grooves_sup:start_link().

%% @spec start() -> ok
%% @doc Start the two_grooves server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(two_grooves).

%% @spec stop() -> ok
%% @doc Stop the two_grooves server.
stop() ->
    Res = application:stop(two_grooves),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
