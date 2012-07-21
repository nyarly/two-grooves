%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%   The simple 1-1 supervisor for snake_games
%%% @end
%%% Created :  Sat May 26 12:22:31 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(game_soop).
-behaviour(supervisor).

%% API
-export([start_link/0, start_game/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_game(Rules, Parlor, Table) ->
  supervisor:start_child(?SERVER, [Rules, Parlor, Table]).

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

  %% @todo Read the OTP Design Priciples re: 'dynamic' modules
  AChild = {gen_game, {gen_game, start_link, []},
    Restart, Shutdown, Type, [gen_game]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
