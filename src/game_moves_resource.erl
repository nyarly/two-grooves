%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Webmachine resource to manage Snake Games
%%% @end
%%% Created :  Tue May 29 02:49:15 2012 by Judson Lester
-module(game_moves_resource).
-author("Judson Lester nyarly@gmail.com").
-export([init/1, allowed_methods/2, post_is_create/2, create_path/2, content_types_accepted/2, from_www_form/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/game_resource.hrl").

init(Config) ->
  game_resource_common:init(Config).

allowed_methods(ReqData, Context) ->
  {['POST','PUT'], ReqData, Context}.

post_is_create(ReqData, Context) ->
  {true, ReqData, Context}.

create_path(ReqData, Context) ->
  {"new", ReqData, Context}.

content_types_accepted(ReqData, Context) ->
  {[{"application/x-www-form-urlencoded", from_www_form}], ReqData, Context}.

from_www_form(ReqData, Context) ->
  {Id, []} = string:to_integer(wrq:path_info(id, ReqData)),
  Player = wrq:path_info(player, ReqData),
  {ok, Game} = game_manager:find_game(Id),

  Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  Move = two_grooves_util:decode_query((Context#game_context.rules):options_format(move), Params),
  case gen_game:move(Game, Player, Move) of
    {ok, _} ->
      GamePath = game_resource_common:build_path(single, Context, [{Field, wrq:path_info(Field, ReqData)} || Field <- [id, player]]),
      {{respond, 303}, wrq:set_resp_header("Location", GamePath, ReqData), Context};
    {error, Error} ->
      {{halt, 400}, invalid_move(Error, ReqData), Context}
  end.

invalid_move(Error, ReqData) ->
  {ok, Body} = game_moves_html_dtl:render([{error, Error}, {game_id, wrq:path_info(id, ReqData)}]),
  wrq:set_resp_body(Body, ReqData).
