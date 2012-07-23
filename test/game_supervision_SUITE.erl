%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Tests for the web component for Snake Game
%%% @end
%%% Created :  Thu Jun 14 20:46:41 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(game_supervision_SUITE).
%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
suite() ->
  [{timetrap,{seconds,10}}].
init_per_suite(Config) ->
  application:start(inets),
  application:start(crypto),
  application:start(mochiweb),
  Config.
end_per_suite(_Config) ->
  application:stop(mochiweb),
  application:stop(crypto),
  application:stop(inets),
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  application:start(webmachine),
  Config.

end_per_testcase(_TestCase, _Config) ->
  application:stop(webmachine),
  ok.

all() ->
  [start_two_grooves_sup, start_top, start_manager, start_soop].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_two_grooves_sup(_Config) ->
  {ok, _} = two_grooves_sup:start_link(),
  ok.

start_top(_Config) ->
  {ok, _} = game_top:start_link(),
  ok.

start_manager(_Config) ->
  {ok, _} = game_manager:start_link(),
  ok.

start_soop(_Config) ->
  {ok, _} = game_soop:start_link(),
  ok.
