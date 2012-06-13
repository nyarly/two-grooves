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
  [score_around_post, score_through_gate, cant_skip_spaces, cant_double_back, cant_leave_board,
  only_five_moves, always_scorable].

score_through_gate() ->
  [].
score_through_gate(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 3,5),
  {ok, _} = snake_game:make_move(Board, 3,4),
  {ok, _} = snake_game:make_move(Board, 3,3),
  {ok, _} = snake_game:make_move(Board, 2,3),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {ok, 3} = snake_game:get_score(Board).

score_around_post() ->
  [].
score_around_post(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 1,1),
  {ok, _} = snake_game:make_move(Board, 1,2),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {ok, _} = snake_game:make_move(Board, 2,3),
  {ok, _} = snake_game:make_move(Board, 2,4),
  {ok, 5} = snake_game:get_score(Board).

cant_skip_spaces() ->
  [].
cant_skip_spaces(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 3,3),
  {error, invalid_move} = snake_game:make_move(Board, 1,3).

cant_double_back(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 2,3),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {error, invalid_move} = snake_game:make_move(Board, 2,3).

cant_leave_board(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 5,5),
  {error, invalid_move} = snake_game:make_move(Board, 5,6).

only_five_moves(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 1,1),
  {ok, _} = snake_game:make_move(Board, 1,2),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {ok, _} = snake_game:make_move(Board, 2,3),
  {ok, _} = snake_game:make_move(Board, 3,3),
  {error, invalid_move} = snake_game:make_move(Board, 3,4).

always_scorable(Config) ->
  Board = ?config(board, Config),
  {ok, 0} = snake_game:get_score(Board).
