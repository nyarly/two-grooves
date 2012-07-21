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
  {ok, Board} = gen_game:start(snakegame_rules, {}, [3,7,undefined]),
  {ok, started} = gen_game:join(Board, a_player, []),
  [{board, Board}, {feature_count, 3}, {player, a_player} | Config];
init_per_group(board_3x3, Config) ->
  {ok, Board} = gen_game:start(snakegame_rules, {}, [3,3,undefined]),
  {ok, started} = gen_game:join(Board, a_player, []),
  [{board, Board}, {feature_count, 2}, {player, a_player} | Config];
init_per_group(board_7x3, Config) ->
  {ok, Board} = gen_game:start(snakegame_rules, {}, [7,3,undefined]),
  {ok, started} = gen_game:join(Board, a_player, []),
  [{board, Board}, {feature_count, 3}, {player, a_player} | Config];
init_per_group(board_20x20, Config) ->
  {ok, Board} = gen_game:start(snakegame_rules, {}, [20,20,undefined]),
  {ok, started} = gen_game:join(Board, a_player, []),
  [{board, Board}, {feature_count, 30}, {player, a_player} | Config];
init_per_group(_GroupName, Config) ->
  Config.

end_per_group(board_3x7, Config) ->
  gen_game:quit(?config(board, Config));
end_per_group(board_3x3, Config) ->
  gen_game:quit(?config(board, Config));
end_per_group(board_7x3, Config) ->
  gen_game:quit(?config(board, Config));
end_per_group(board_20x20, Config) ->
  gen_game:quit(?config(board, Config));
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
  {ok, Board} = gen_game:start(snakegame_rules, {}, [5,5,[{post, {2,2}}, {gate, {3,3},{3,4}}]]),
  {ok, started} = gen_game:join(Board, a_player, []),
  [{board, Board}, {player, a_player} | Config];
init_per_testcase_for_group(_, _TestCase, Config) ->
  Config.

init_per_testcase(enough_features_generated, Config) ->
  Config;
init_per_testcase(all_features_within_board, Config) ->
  Config;
init_per_testcase(TestCase, Config) ->
  {ok, Board} = gen_game:start(snakegame_rules, {}, [5,5,undefined]),
  {ok, started} = gen_game:join(Board, a_player, []),
  handle_each_per_group(init_per_testcase_for_group, TestCase, [{board, Board}, {player, a_player} | Config]).

end_per_testcase(enough_features_generated, Config) ->
  Config;
end_per_testcase(all_features_within_board, Config) ->
  Config;
end_per_testcase(_TestCase, Config) ->
  gen_game:quit(?config(board, Config)),
  ok.

groups() ->
  [
    {board_3x7,   [], [all_features_within_board, enough_features_generated]},
    {board_3x3,   [], [all_features_within_board, enough_features_generated]},
    {board_7x3,   [], [all_features_within_board, enough_features_generated]},
    {board_20x20, [], [all_features_within_board, enough_features_generated]},
    {board_sizes, [{repeat_until_any_fail, 3}], [{group, board_3x7}, {group, board_3x3}, {group, board_7x3}, {group, board_20x20}]},
    {feature_scoring, [], [score_around_post, score_through_gate]}
  ].
all() ->
  [{group, board_sizes}, {group, feature_scoring}, cant_skip_spaces, cant_double_back, cant_leave_board,
  only_five_moves, always_scorable].

cant_skip_spaces(Config) ->
  Board = ?config(board, Config),
  Player = ?config(player, Config),
  {ok, _} = gen_game:move(Board, Player, [3,3]),
  {error, _} = gen_game:move(Board, Player, [1,3]).

cant_double_back(Config) ->
  Board = ?config(board, Config),
  Player = ?config(player, Config),
  {ok, _} = gen_game:move(Board, Player, [2,3]),
  {ok, _} = gen_game:move(Board, Player, [2,2]),
  {error, _} = gen_game:move(Board, Player, [2,3]).

cant_leave_board(Config) ->
  Board = ?config(board, Config),
  Player = ?config(player, Config),
  {ok, _} = gen_game:move(Board, Player, [5,5]),
  {error, _} = gen_game:move(Board, Player, [5,6]).

only_five_moves(Config) ->
  Board = ?config(board, Config),
  Player = ?config(player, Config),
  {ok, _} = gen_game:move(Board, Player, [1,1]),
  {ok, _} = gen_game:move(Board, Player, [1,2]),
  {ok, _} = gen_game:move(Board, Player, [2,2]),
  {ok, _} = gen_game:move(Board, Player, [2,3]),
  {ok, _} = gen_game:move(Board, Player, [3,3]),
  {error, {bad_state,move,finished}} = gen_game:move(Board, Player, [3,4]).

always_scorable(Config) ->
  Board = ?config(board, Config),
  {ok, [{_, 0}]} = gen_game:score(Board).

all_features_within_board(Config) ->
  Board = ?config(board, Config),
  Player = ?config(player, Config),
  {ok, List} = gen_game:to_proplist(Board, Player),
  Features = proplists:get_value(targets, List),
  Size = proplists:get_value(dims, List),
  DimX = proplists:get_value(x, Size),
  DimY = proplists:get_value(y, Size),
  [ case proplists:get_value(type, Feature) of
      post ->
        ?assertFun(fun(Val) ->    0 =< Val end, proplists:get_value(x, proplists:get_value(around, Feature))),
        ?assertFun(fun(Val) ->    0 =< Val end, proplists:get_value(y, proplists:get_value(around, Feature))),
        ?assertFun(fun(Val) -> DimX >= Val end, proplists:get_value(x, proplists:get_value(around, Feature))),
        ?assertFun(fun(Val) -> DimY >= Val end, proplists:get_value(y, proplists:get_value(around, Feature)));
      gate ->
        [Here, There] = proplists:get_value(between, Feature),
        case {?config(x, Here), ?config(y, Here), ?config(x, There), ?config(y, There)} of
          {X, Hy, X, Ty} -> 1 = abs(Hy - Ty);
          {Hx, Y, Tx, Y} -> 1 = abs(Hx - Tx)
        end,
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
  Player = ?config(player, Config),
  {ok, List} = gen_game:to_proplist(Board, Player),
  Features = proplists:get_value(targets, List),
  true = length(Features) >= ?config(feature_count, Config).

score_two_for_one(Config) ->
  {ok, Board} = gen_game:start(snakegame_rules, [], [5,5,[{gate, {3,3},{4,3}}, {gate, {3,3},{3,4}}]]),
  Player = ?config(player, Config),
  {ok, _} = gen_game:move(Board, Player, [3,4]),
  {ok, _} = gen_game:move(Board, Player, [3,3]),
  {ok, _} = gen_game:move(Board, Player, [4,3]),
  {ok, [{_, 6}]} = gen_game:score(Board),
  gen_game:quit(Board).

score_through_gate(Config) ->
  Board = ?config(board, Config),
  Player = ?config(player, Config),
  {ok, _} = gen_game:move(Board, Player, [3,4]),
  {ok, _} = gen_game:move(Board, Player, [3,3]),
  {ok, [{_, 3}]} = gen_game:score(Board).

score_around_post(Config) ->
  Board = ?config(board, Config),
  Player = ?config(player, Config),
  {ok, _} = gen_game:move(Board, Player, [1,1]),
  {ok, _} = gen_game:move(Board, Player, [1,2]),
  {ok, _} = gen_game:move(Board, Player, [2,2]),
  {ok, [{_, 5}]} = gen_game:score(Board).
