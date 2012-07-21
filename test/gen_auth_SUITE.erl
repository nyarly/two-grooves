%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Tests for gen_auth - an authorization framework for WM
%%% @end
%%% Created :  Thu Jul 12 14:47:10 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(gen_auth_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("assertions.hrl").

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
  Config.
end_per_testcase(_TestCase, _Config) ->
  ok.
groups() ->
  [].
all() ->
  [
    challenges_list,
    player_authorized_to_move_in_game,
    player_authorized_to_create_a_game,
    player_authorized_to_join_a_game,
    player_denied_move_for_other_player,
    other_player_denied_move_for_player,
    wrong_password_denied,
    no_password_denied
  ].

challenges_list(_Config) ->
  "Basic realm='Games'" = gen_auth:challenges().

player_authorized_to_move_in_game(_Config) ->
  authorized = gen_auth:authorize("Basic " ++ base64:encode_to_string("player:password"), {{game, 1}, {player, "player"}}, move).

player_authorized_to_create_a_game(_Config) ->
  authorized = gen_auth:authorize("Basic " ++ base64:encode_to_string("player:password"), {{game, new}}, create).

player_authorized_to_join_a_game(_Config) ->
  authorized = gen_auth:authorize("Basic " ++ base64:encode_to_string("player:password"), {{game, 1}}, join).

player_denied_move_for_other_player(_Config) ->
  {denied, no_grant} = gen_auth:authorize("Basic " ++ base64:encode_to_string("player:password"), {{game, 1}, {player, "other_player"}}, move).

other_player_denied_move_for_player(_Config) ->
  {denied, no_grant} = gen_auth:authorize("Basic " ++ base64:encode_to_string("other_player:password"), {{game, 1}, {player, "player"}}, move).

wrong_password_denied(_Config) ->
  {denied, bad_authentication} = gen_auth:authorize("Basic " ++ base64:encode_to_string("player:not_password"), {{game, new}}, create).

no_password_denied(_Config) ->
  {denied, not_authenticated} = gen_auth:authorize(null, {{game, new}}, create).
