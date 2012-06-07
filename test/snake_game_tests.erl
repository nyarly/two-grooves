%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Tests for snake_game
%%% @end
%%% Created :  Thu Jun 07 00:25:11 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(snake_game_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  {ok, Board} = snake_game:start_game({5,5}),
  Board.

teardown(Board) ->
  catch(snake_game:quit(Board)).

game_test_() ->
  {
    setup,
    fun setup/0,
    fun teardown/1,
    fun score_around_post/1
  }.

score_around_post(Board) ->
  snake_game:make_move(Board, 1,1),
  snake_game:make_move(Board, 1,2),
  snake_game:make_move(Board, 2,2),
  snake_game:make_move(Board, 2,3),
  snake_game:make_move(Board, 2,4),
  [?_assertEqual({ok,5}, snake_game:get_score(Board))].
