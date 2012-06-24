%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Webmachine resource to manage Snake Games
%%% @end
%%% Created :  Tue May 29 02:49:15 2012 by Judson Lester
-module(snakegames_resource).
-author("Judson Lester nyarly@gmail.com").
-export([init/1, get_dispatches/0, to_resource/2, to_html/2, post_is_create/2, create_path/2, content_types_accepted/2, from_www_form/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
  {ok, Config}.

get_dispatches() ->
  [
    {["snakegames"], ?MODULE, []}
  ].

post_is_create(ReqData, Context)->
  {true, ReqData, Context}.

create_path(ReqData, Context) ->
  {ok, NextId} = snake_game_manager:next_id(),
  {io_lib:format("~p", [NextId]),
    ReqData, [{target_id, NextId} | Context]}.

content_types_accepted(ReqData, Context) ->
  {[{"application/x-www-form-urlencoded", from_www_form}], ReqData, Context}.

from_www_form(ReqData, Context) ->
  case(proplists:get_value(target_id, Context)) of
    undefined -> {error, "Posting values without target_id"};
    TargetId ->
      Params = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
      {Width, []} = string:to_integer(proplists:get_value("width", Params)),
      {Height, []} = string:to_integer(proplists:get_value("height", Params)),
      snake_game_manager:create_game(TargetId, {Width, Height}),
      {{respond, 303}, ReqData, Context}
  end.

to_resource(_ReqData, []) ->
  [{ids, snake_game_manager:list_games()}].

to_html(ReqData, Context) ->
  Result = snakegames_html_dtl:render(to_resource(ReqData, Context)),
  {Result, ReqData, Context}.
