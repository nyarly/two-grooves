%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Webmachine resource to manage Snake Games
%%% @end
%%% Created :  Tue May 29 02:49:15 2012 by Judson Lester
-module(snakegame_resource).
-author("Judson Lester nyarly@gmail.com").
-export([init/1, get_dispatches/0, to_resource/2, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
  {ok, Config}.

get_dispatches() ->
  [
    {["snakegame", id], ?MODULE, []}
  ].

to_resource(ReqData, _Context) ->
  {Index, []} = string:to_integer(wrq:path_info(id, ReqData)),
  {ok, Game} = snake_game_manager:find_game(Index),
  {ok, Proplist} = snake_game:to_proplist(Game),
  Proplist.

to_html(ReqData, Context) ->
  Result = snakegame_html_dtl:render(to_resource(ReqData, Context)),
  {Result, ReqData, Context}.
