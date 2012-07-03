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
-include("assertions.hrl").

suite() ->
  [{timetrap,{seconds,30}}].
init_per_suite(Config) ->
  Config.
end_per_suite(_Config) ->
  ok.

init_per_group(board_3x7, Config) ->
  {ok, Board} = snake_game:start_game({3,7}),
  [{board, Board}, {feature_count, 3} | Config];
init_per_group(board_3x3, Config) ->
  {ok, Board} = snake_game:start_game({3,3}),
  [{board, Board}, {feature_count, 2} | Config];
init_per_group(board_7x3, Config) ->
  {ok, Board} = snake_game:start_game({7,3}),
  [{board, Board}, {feature_count, 3} | Config];
init_per_group(board_20x20, Config) ->
  {ok, Board} = snake_game:start_game({20,20}),
  [{board, Board}, {feature_count, 30} | Config];
init_per_group(_GroupName, Config) ->
  Config.

end_per_group(board_3x7, Config) ->
  snake_game:quit(?config(board, Config)),
  ok;
end_per_group(board_3x3, Config) ->
  snake_game:quit(?config(board, Config)),
  ok;
end_per_group(board_7x3, Config) ->
  snake_game:quit(?config(board, Config)),
  ok;
end_per_group(board_20x20, Config) ->
  snake_game:quit(?config(board, Config)),
  ok;
end_per_group(_GroupName, _Config) ->
  ok.

handle_each_per_group(Handler, TestCase, Config) ->
  case proplists:get_value(tc_group_properties,Config) of
    undefined -> Config;
    GroupProperties ->
      case proplists:get_value(name,GroupProperties) of
        undefined -> Config;
        GroupName ->
          ?MODULE:Handler(GroupName, TestCase, Config)
      end
  end.

init_per_testcase_for_group(feature_scoring, _TestCase, Config) ->
  {ok, Board} = snake_game:start_game({with_features, [{post, {2,2}}, {gate, {3,3},{3,4}}]}),
  [{board, Board} | Config];
init_per_testcase_for_group(_, _TestCase, Config) ->
  Config.

init_per_testcase(enough_features_generated, Config) ->
  Config;
init_per_testcase(all_features_within_board, Config) ->
  Config;
init_per_testcase(TestCase, Config) ->
  {ok, Board} = snake_game:start_game({5,5}),
  handle_each_per_group(init_per_testcase_for_group, TestCase, [{board, Board} | Config]).

end_per_testcase(enough_features_generated, Config) ->
  ok;
end_per_testcase(all_features_within_board, Config) ->
  ok;
end_per_testcase(_TestCase, Config) ->
  snake_game:quit(?config(board, Config)),
  ok.

groups() ->
  [
    {board_3x7,   [], [all_features_within_board, enough_features_generated]},
    {board_3x3,   [], [all_features_within_board, enough_features_generated]},
    {board_7x3,   [], [all_features_within_board, enough_features_generated]},
    {board_20x20, [], [all_features_within_board, enough_features_generated]},
    {board_sizes, [{repeat_until_any_fail, 10}], [{group, board_3x7}, {group, board_3x3}, {group, board_7x3}, {group, board_20x20}]},
    {feature_scoring, [], [score_around_post, score_through_gate]}
  ].
all() ->
  [{group, board_sizes}, {group, feature_scoring}, cant_skip_spaces, cant_double_back, cant_leave_board,
  only_five_moves, always_scorable, build_7x4_board].

cant_skip_spaces(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 3,3),
  {error, _} = snake_game:make_move(Board, 1,3).

cant_double_back(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 2,3),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {error, _} = snake_game:make_move(Board, 2,3).

cant_leave_board(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 5,5),
  {error, _} = snake_game:make_move(Board, 5,6).

only_five_moves(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 1,1),
  {ok, _} = snake_game:make_move(Board, 1,2),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {ok, _} = snake_game:make_move(Board, 2,3),
  {ok, _} = snake_game:make_move(Board, 3,3),
  {error, _} = snake_game:make_move(Board, 3,4).

always_scorable(Config) ->
  Board = ?config(board, Config),
  {ok, 0} = snake_game:get_score(Board).

build_7x4_board(_Config) ->
  {ok, Board} = snake_game:start_game({7,4}),
  {ok, _} = snake_game:make_move(Board, 7,4).

all_features_within_board(Config) ->
  Board = ?config(board, Config),
  {ok, List} = snake_game:to_proplist(Board),
  Features = proplists:get_value(targets, List),
  Size = proplists:get_value(dims, List),
  DimX = proplists:get_value(x, Size),
  DimY = proplists:get_value(y, Size),
  [ case proplists:get_value(type, Feature) of
      post ->
        ?assertFun(fun(Val) ->         0 < Val end, proplists:get_value(x, proplists:get_value(around, Feature))),
        ?assertFun(fun(Val) ->         0 < Val end, proplists:get_value(y, proplists:get_value(around, Feature))),
        ?assertFun(fun(Val) -> DimX + 1 >= Val end, proplists:get_value(x, proplists:get_value(around, Feature))),
        ?assertFun(fun(Val) -> DimY + 1 >= Val end, proplists:get_value(y, proplists:get_value(around, Feature)));
      gate ->
        [Here, There] = proplists:get_value(between, Feature),
        ?assertFun(fun(Val) ->     0 < Val end, proplists:get_value(x, Here)),
        ?assertFun(fun(Val) ->     0 < Val end, proplists:get_value(y, Here)),
        ?assertFun(fun(Val) ->     0 < Val end, proplists:get_value(x, There)),
        ?assertFun(fun(Val) ->     0 < Val end, proplists:get_value(y, There)),
        ?assertFun(fun(Val) -> DimX >= Val end, proplists:get_value(x, Here)),
        ?assertFun(fun(Val) -> DimY >= Val end, proplists:get_value(y, Here)),
        ?assertFun(fun(Val) -> DimX >= Val end, proplists:get_value(x, There)),
        ?assertFun(fun(Val) -> DimY >= Val end, proplists:get_value(y, There))
    end || Feature <- Features].

enough_features_generated(Config) ->
  Board = ?config(board, Config),
  {ok, List} = snake_game:to_proplist(Board),
  Features = proplists:get_value(targets, List),
  ct:pal("Expecting count ~p in features: ~p~n", [?config(feature_count, Config), Features]),
  true = length(Features) >= ?config(feature_count, Config).

score_through_gate(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 3,4),
  {ok, _} = snake_game:make_move(Board, 3,3),
  {ok, 3} = snake_game:get_score(Board).

score_around_post(Config) ->
  Board = ?config(board, Config),
  {ok, _} = snake_game:make_move(Board, 1,1),
  {ok, _} = snake_game:make_move(Board, 1,2),
  {ok, _} = snake_game:make_move(Board, 2,2),
  {ok, 5} = snake_game:get_score(Board).
