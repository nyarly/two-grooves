%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Webmachine resource to manage Snake Games
%%% @end
%%% Created :  Tue May 29 02:49:15 2012 by Judson Lester
-module(snakegame_moves_resource).
-author("Judson Lester nyarly@gmail.com").
-export([init/1, get_dispatches/0, allowed_methods/2, post_is_create/2, create_path/2, content_types_accepted/2, from_www_form/2]).

-include_lib("webmachine/include/webmachine.hrl").

get_dispatches() ->
  [
    {["snakegame", id, "moves"], ?MODULE, []}
  ].

init(Config) ->
  {ok, Config}.

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
  {ok, Game} = snake_game_manager:find_game(Id),

  Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  {X, []} = string:to_integer(proplists:get_value("x", Params)),
  {Y, []} = string:to_integer(proplists:get_value("y", Params)),

  snake_game:make_move(Game, X, Y),
  {{respond, 303}, wrq:set_resp_header("Location", io_lib:format("/snakegame/~s",[wrq:path_info(id, ReqData)]), ReqData), Context}.
