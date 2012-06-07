%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012, Judson Lester. All Rights Reserved.
%%% @doc
%%%		Webmachine resource to manage Snake Games
%%% @end
%%% Created :  Tue May 29 02:49:15 2012 by Judson Lester
-module(snake_game_resource).
-author("Judson Lester nyarly@gmail.com").
-export([init/1, get_dispatches/0, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
  {ok, Config}.

get_dispatches() ->
  [
    {["snake_game"], ?MODULE, [index]},
    {["snake_game", id], ?MODULE, []}
  ].

to_html(ReqData, Context = [index]) ->
  Result = "snake game index",
  {Result, ReqData, Context};
to_html(ReqData, Context) ->
  Result = "a snake game",
  {Result, ReqData, Context}.
