%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Test suite for Snake Game
%%% @end
%%% Created :  Tue Jun 12 23:29:23 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(snake_game_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
suite() ->
  [{timetrap,{seconds,30}}].
init_per_suite(Config) ->
  Config.
end_per_suite(_Config) ->
  ok.
init_per_group(_GroupName, Config) ->
  Config.
end_per_group(_GroupName, _Config) ->
  ok.
init_per_testcase(_TestCase, Config) ->
  {ok, Board} = snake_game:start_game({5,5}),
  [{board, Board} | Config].
end_per_testcase(_TestCase, Config) ->
  snake_game:quit(?config(board, Config)),
  ok.
groups() ->
  [].
all() ->
  [score_around_post].
score_around_post() ->
  [].
score_around_post(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 1,1),
  {ok, _} = snake_game:make_move(Board, 1,2),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {ok, _} = snake_game:make_move(Board, 2,3),
  {ok, _} = snake_game:make_move(Board, 2,4),
  {ok, 5}      = snake_game:get_score(Board).
