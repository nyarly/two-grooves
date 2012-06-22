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
  StartResult = application:start(webmachine),
  case(?config(tc_group_properties, Config)) of
    undefined -> ok;
    GroupProperties ->
      init_per_testcase_by_group(?config(name, GroupProperties), Config)
  end.

init_per_testcase_by_group(web, Config) ->
  {ok, TopPid} = snake_game_top:start_link(),
  {ok, DispatchList} = application:get_env(webmachine, dispatch_list),
  [
    {top_pid, TopPid},
    {dispatch_list, DispatchList}
    | Config
  ]
  ;
init_per_testcase_by_group(top, Config) ->
  Config.

end_per_testcase(_TestCase, Config) ->
  application:stop(webmachine),
  case(?config(top_pid, Config)) of
    undefined -> ok;
    TopPid -> exit(TopPid, normal)
  end,
  ok.

groups() ->
  [
    {web, [], [index_dispatches, play_a_game]},
    {top, [], [start_top, start_manager, start_soop]}
  ].
all() ->
  [{group, web}, {group, top}].

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
  Scheme = "http",
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

setup_GET(Path, Headers, Config) ->
  {_Mod, _Options, RD} = dispatch_request(?config(dispatch_list, Config), build_request('GET', Path, Headers)),
  RD.

setup_POST(Path, Headers, Body, Config) ->
  {_Mod, _Options, RD} = dispatch_request(?config(dispatch_list, Config), build_request('POST', Path, Headers, Body)),
  RD.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

index_dispatches() ->
  [].
index_dispatches(Config) ->
  RD = build_request('POST', "snakegame/", [], "width=5&height=5"),
  {snake_game_resource, [index], _RD2} = dispatch_request(?config(dispatch_list, Config), RD).

play_a_game() ->
  [].
play_a_game(Config) ->
  %That is:
  %POST a new game
  NewGameRequest = setup_POST("snakegame/", [], "width=5&height=5", Config),
  {true, NewGameRequest, [index]} = snake_game_resource:post_is_create(NewGameRequest, [index]),
  {NewPath, NewGameRequest, CreateContext} = snake_game_resource:create_path(NewGameRequest, [index]),
  NewGameUpdate = wrq:set_disp_path(NewPath, NewGameRequest),
  {AcceptedTypes, NewGameUpdate, CreateContext} = snake_game_resource:content_types_accepted(NewGameUpdate, CreateContext),
  [from_www_form] = [ Fun || {Type, Fun} <- AcceptedTypes, "application/x-www-form-urlencoded" =:= Type],
  %Response should redirect to the URL of the new game
  {{respond, 303}, _RedirectedRequest, CreateContext} = snake_game_resource:from_www_form(NewGameUpdate, CreateContext),

  %GET from that URL
  SeeNewGame = setup_GET("snakegame/1", [], Config),
  GameResource = snake_game_resource:to_resource(SeeNewGame, []),
  ct:pal("~p", [[GameResource]]),
  [] = proplists:get_value(moves, GameResource),
  {5,5} = proplists:get_value(dims, GameResource),
  0 = proplists:get_value(score, GameResource),
  false = undefined =:= proplists:get_value(targets, GameResource),
  %That should be a game
  ok.

start_top(_Config) ->
  {ok, _} = snake_game_top:start_link(),
  ok.

start_manager(_Config) ->
  {ok, _} = snake_game_manager:start_link(),
  ok.

start_soop(_Config) ->
  {ok, _} = snake_game_soop:start_link(),
  ok.
