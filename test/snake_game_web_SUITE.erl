%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Tests for the web component for Snake Game
%%% @end
%%% Created :  Thu Jun 14 20:46:41 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(snake_game_web_SUITE).
%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("webmachine/include/wm_reqstate.hrl").
%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
suite() ->
  [{timetrap,{minutes,10}}].
init_per_suite(Config) ->
  ok = two_grooves:start(),
  Config.
end_per_suite(_Config) ->
  two_grooves:stop(),
  ok.

init_per_group(_GroupName, Config) ->
  bounce_game_top(),
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  {ok, DispatchList} = application:get_env(webmachine, dispatch_list),
  [ {dispatch_list, DispatchList} | Config ].

end_per_testcase(_TestCase, Config) ->
  ok.

bounce_game_top() ->
  supervisor:terminate_child(two_grooves_sup, game_top),
  case supervisor:restart_child(two_grooves_sup, game_top) of
    {ok, _Child} -> ok;
    {ok, _Child, _Info} -> ok;
    {error, running} -> ok
  end.

%%--------------------------------------------------------------------
%%Helpers
%%--------------------------------------------------------------------

%% Here's what I think now:
%% These helpers should operate on a Webmachine request module,
%% And eventually return a ReqData.

build_request(Method, Path) ->
  build_request(Method, Path, gb_trees:empty()).

build_request(Method, Path, Headers, Body) ->
  BaseRD = build_request(Method, Path, Headers),
  add_request_body(Body, BaseRD).

build_request(Method, Path, Headers) ->
  Version = {1,1},
  MochiReq = mochiweb_request:new(testing, Method, Path, Version, gb_trees:from_orddict(Headers)),
  WMReq = webmachine:new_request(mochiweb,MochiReq),
  {RD, RS} = WMReq:get_reqdata(),
  RD#wm_reqdata{wm_state=RS}.

add_request_body(Body, RD) ->
  NewReqState = RD#wm_reqdata.wm_state#wm_reqstate{bodyfetch=standard,reqbody=Body},
  RD#wm_reqdata{wm_state=NewReqState, req_body=Body}.

dispatch_request(DispatchList, RD) ->
  Host = "example.com",
  Path = wrq:raw_path(RD),
  {ExpectedModule, ExpectedOptions, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath} =
  webmachine_dispatcher:dispatch(Host, Path, DispatchList, RD),
  {ExpectedModule, ExpectedOptions, wrq:load_dispatch_data(dict:from_list(Bindings),HostTokens,Port,PathTokens,AppRoot,StringPath, RD)}.

setup_request(Request, TestConfig) ->
  {Module, DispatchConfig, ReqData} = dispatch_request(?config(dispatch_list, TestConfig), Request),
  {ok, Context} = Module:init(DispatchConfig),
  {Module, ReqData, Context}.

setup_GET(Path, Headers, Config) ->
  setup_request(build_request('GET', Path, Headers), Config).

setup_POST(Path, Headers, Body, Config) ->
  setup_request(build_request('POST', Path, Headers, Body), Config).

examine_game(ExpectedMoves, ExpectedDims, ExpectedScore, Config) ->
  {game_resource, SeeNewGame, Context} = setup_GET("snakegame/1/player1", [], Config),
  GameResource = game_resource:to_resource(SeeNewGame, Context),
  ct:pal("Game: ~p~n", [GameResource]),
  ExpectedMoves = proplists:get_value(moves, GameResource),
  ExpectedDims = proplists:get_value(dims, GameResource),
  ExpectedScore = proplists:get_value(score, GameResource),
  false = undefined =:= proplists:get_value(targets, GameResource),
  {HTML, SeeNewGame, Context} = game_resource:to_html(SeeNewGame, Context),
  true = is_list(HTML).

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

groups() ->
  [
    {dispatches, [], [index_dispatches]},
    {play_a_game, [sequence], [create_a_game, game_has_no_moves, make_a_move, game_has_one_move]}
  ].
all() ->
  [index_renders, {group, dispatches}, {group, play_a_game}].

index_dispatches(Config) ->
  RD = build_request('POST', "snakegames/", [], "width=5&height=5"),
  {games_resource, PropList, _RD2} = dispatch_request(?config(dispatch_list, Config), RD),
  snakegame_rules = proplists:get_value(rules_module, PropList).

index_renders(Config) ->
  {games_resource, ViewListRequest, Context} = setup_GET("snakegames/", [], Config),
  {HTML, ViewListRequest, Context} = games_resource:to_html(ViewListRequest, Context),
  true = is_list(HTML).

create_a_game(Config) ->
  {games_resource, NewGameRequest, Context} = setup_POST("snakegames/", [], "player=player1&width=5&height=5", Config),
  {true, NewGameRequest, Context} = games_resource:post_is_create(NewGameRequest, Context),
  {NewPath, ViewNewGameRequest, Context} = games_resource:create_path(NewGameRequest, Context),
  NewGameUpdate = wrq:set_disp_path(NewPath, ViewNewGameRequest),
  {AcceptedTypes, NewGameUpdate, Context} = games_resource:content_types_accepted(NewGameUpdate, Context),
  [from_www_form] = [ Fun || {Type, Fun} <- AcceptedTypes, "application/x-www-form-urlencoded" =:= Type],
  {{respond, 303}, _RedirectedRequest, Context} = games_resource:from_www_form(NewGameUpdate, Context).

game_has_no_moves(Config) ->
  examine_game([], [{x,5},{y,5}], 0, Config).

make_a_move(Config) ->
  {game_moves_resource, MakeAMove, Context} = setup_POST("snakegame/1/player1/moves", [], "x=3&y=3", Config),
  {true, MakeAMove, Context} = game_moves_resource:post_is_create(MakeAMove, Context),
  {NewMovePath, MakeAMove, Context} = game_moves_resource:create_path(MakeAMove, Context),
  MadeAMove = wrq:set_disp_path(NewMovePath, MakeAMove),
  {AcceptedMoveTypes, MadeAMove, Context} = game_moves_resource:content_types_accepted(MadeAMove, Context),
  [from_www_form] = [ Fun || {Type, Fun} <- AcceptedMoveTypes, "application/x-www-form-urlencoded" =:= Type],
  {{respond, 303}, _RedirectedRequest2, MoveContext} = game_moves_resource:from_www_form(MadeAMove, Context),
  ok.

game_has_one_move(Config) ->
  examine_game([[{x,3},{y,3}]], [{x,5},{y,5}], 0, Config).
