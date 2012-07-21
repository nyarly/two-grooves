%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Webmachine resource to manage Games
%%% @end
%%% Created :  Tue May 29 02:49:15 2012 by Judson Lester
-module(games_resource).
-author("Judson Lester nyarly@gmail.com").
-export([init/1, allowed_methods/2, to_resource/2, to_html/2, post_is_create/2,
    create_path/2, content_types_accepted/2, from_www_form/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("include/game_resource.hrl").

init(Config) ->
  [RulesModule, ParlorOpts, ResourceOpts, Paths, Dispatches] =
  [proplists:get_value(Field, Config) || Field <- [rules_module, parlor_opts, resource_opts, paths, dispatches]],
  {ok, #game_context{rules=RulesModule, parlor=ParlorOpts, resource=ResourceOpts, paths=Paths, tag_context=[{dispatches, Dispatches}]}}.

allowed_methods(ReqData, Context) ->
  {['GET', 'POST', 'HEAD'], ReqData, Context}.

post_is_create(ReqData, Context)->
  {true, ReqData, Context}.

create_path(ReqData, Context) ->
  {ok, NextId} = game_manager:next_id(),
  {io_lib:format("~p", [NextId]), wrq:add_note(target_id, NextId, ReqData), Context}.

content_types_accepted(ReqData, Context) ->
  {[{"application/x-www-form-urlencoded", from_www_form}], ReqData, Context}.

from_www_form(ReqData, Context) ->
  case proplists:get_value(target_id, wrq:get_notes(ReqData)) of
    undefined -> {error, "Posting values without target_id"};
    TargetId ->
      Rules = Context#game_context.rules,
      Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
      Player = proplists:get_value("player", Params),
      ParlorOpts = Context#game_context.parlor,
      TableOpts = two_grooves_util:decode_query(Rules:options_format(table), Params),
      PlayerOpts = two_grooves_util:decode_query(Rules:options_format(player), Params),
      {ok, NewGame} = game_manager:create_game(TargetId, Rules, ParlorOpts, TableOpts),
      {ok, _} = gen_game:join(NewGame, Player, PlayerOpts),
      {{respond, 303}, ReqData, Context}
  end.

to_resource(_ReqData, Context) ->
  {ok, Games} = game_manager:list_games(),
  [ {player, "player"},
    {games, [[{id, Id}] || Id <- Games]},
    {parlor_opts, Context#game_context.parlor},
    {resource_opts, Context#game_context.resource}].

to_html(ReqData, Context) ->
  {ok, Result} = games_html_dtl:render(
    [{paths, Context#game_context.paths}, {resource, to_resource(ReqData, Context)}],
    [{custom_tags_context, Context#game_context.tag_context}]),
  {Result, ReqData, Context}.
