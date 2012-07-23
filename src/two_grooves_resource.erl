%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(two_grooves_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

to_resource(_ReqData, _State) ->
  [].

to_html(ReqData, State) ->
  {ok, Body} = root_dtl:render(to_resource(ReqData, State)),
  {Body, ReqData, State}.
