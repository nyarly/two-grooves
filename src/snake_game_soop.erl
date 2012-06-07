%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%   The simple 1-1 supervisor for snake_games
%%% @end
%%% Created :  Sat May 26 12:22:31 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(snake_game_soop).
-behaviour(supervisor).

%% API
-export([start_link/0, start_game/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_game(Size) ->
  supervisor:start_child(?SERVER, [Size]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%supervisor behavior callbacks

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = transient,
  Shutdown = 2000,
  Type = worker,

  AChild = {snake_game, {snake_game, start_link, []},
    Restart, Shutdown, Type, [snake_game]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
