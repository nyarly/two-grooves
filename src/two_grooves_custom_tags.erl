%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Custom ErlyDTL tags for Two Grooves
%%% @end
%%% Created :  Sat Jun 30 03:50:02 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(two_grooves_custom_tags).
%% API
-export([render_tree/2, path_to/2, query_params/2]).
%%%===================================================================
%%% API
%%%===================================================================

render_tree(Args, _Context) ->
  ValueTmpl = binary_to_atom(proplists:get_value(values, Args), latin1),
  ListTmpl = binary_to_atom(proplists:get_value(lists, Args), latin1),
  DictTmpl = binary_to_atom(proplists:get_value(dicts, Args), latin1),
  Tree = proplists:get_value(resource, Args),
  Name = proplists:get_value(name, Args, "resource"),
  render_tree({ValueTmpl, ListTmpl, DictTmpl}, Tree, Name).

%% Usage: {% path_to dispatch=dispatch_list.<name> path_info=<path> %}
path_to(Args, Context) ->
  io:format("~p ~p~n", [Args, Context]),
  {dispatches, Dispatches} = proplists:lookup(dispatches, Context),
  {name, Name} = proplists:lookup(name, Args),
  io:format("~p ~p~n", [Name, Dispatches]),
  {Name, Dispatch} = proplists:lookup(Name, Dispatches),
  PathInfo = proplists:delete(name, Args),
  io:format("~p ~p~n", [Dispatch, PathInfo]),
  [[<<"/">>, Part] || Part <- two_grooves_named_dispatch:zip_dispatch(Dispatch, PathInfo)].

query_params(Args, _Context) ->
  Params = proplists:get_value(params, Args),
  [First | Rest ] = [[Key, <<"=">>, Value] || {Key, Value} <- Params],
  [First | [[<<"&">>, Part] || Part <- Rest]].

%%%===================================================================
%%% Internal functions
%%%===================================================================

render_tree(Tmpls = {ValueTmpl, _, _}, Tree, Name) ->
  case is_dictionary(Tree) of
    true -> render_dict(Tmpls, Tree, Name);
    _ ->
      case is_array(Tree) of
        true -> render_list(Tmpls, Tree, Name);
        _ -> {ok, Result} = ValueTmpl:render([{resource, Tree}, {name, Name}]),
          chomp_whitespace(Result)
      end
  end.

render_list(Tmpls = {_, ListTmpl, _}, Tree, Name) ->
  {ok, Result} = ListTmpl:render([
      {resource, lists:map(fun(Item) -> render_tree(Tmpls, Item, Name) end, Tree)},
      {name, Name}]),
  Result.

render_dict(Tmpls = {_, _, DictTmpl}, Tree, Name) ->
  {ok, Result} = DictTmpl:render([
      {resource, lists:map(fun({Key, Value}) -> {Key, render_tree(Tmpls, Value, Key)} end,
          proplists:unfold(Tree))},
      {name, Name}]),
  Result.

chomp_whitespace(String) ->
  {match, [Capture|_Rest]} = re:run(String, "(.(?!\s*$))*[^\s]", [{capture, first, list}]),
  Capture.

is_dictionary(List) when is_list(List) ->
  is_proplist(List);
is_dictionary(_) ->
  false.

is_array(List) when is_list(List) ->
  not is_dictionary(List) andalso not is_string(List);
is_array(_) ->
  false.

is_proplist(List) ->
  lists:all(fun(Elem) -> is_atom(Elem) orelse is_tuple(Elem) end, List).

is_string(List) ->
  lists:all(fun(Elem) -> is_integer(Elem) end, List).
