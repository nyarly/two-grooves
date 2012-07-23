%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Webmachine resource to manage Snake Games
%%% @end
%%% Created :  Tue May 29 02:49:15 2012 by Judson Lester
-module(game_resource).
-author("Judson Lester nyarly@gmail.com").
-export([init/1, content_types_provided/2, to_resource/2, to_html/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/game_resource.hrl").


init(Config) ->
  game_resource_common:init(Config).

to_resource(ReqData, Context) ->
  {Index, []} = string:to_integer(wrq:path_info(id, ReqData)),
  Player = wrq:path_info(player, ReqData),
  {ok, Game} = game_manager:find_game(Index),
  {ok, Proplist} = gen_game:to_proplist(Game, Player),
  [
    {id, Index},
    {player, Player},
    {parlor_opts, Context#game_context.parlor},
    {resource_opts, Context#game_context.resource}
    | Proplist].

content_types_provided(ReqData, Context) ->
  List = [
    {"text/html", to_html},
    {"application/json", to_json}
  ],
  {List, ReqData, Context}.

to_json(ReqData, Context) ->
  {mochijson2:encode(to_resource(ReqData, Context)), ReqData, Context}.

to_html(ReqData, Context) ->
  {Json, _ReqData, _Context} = to_json(ReqData, Context),
  {game_resource_common:render(game_html_dtl, [{json, Json}, {game, to_resource(ReqData, Context)}], Context), ReqData, Context}.
