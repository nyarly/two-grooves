%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		WM dispatch building for games
%%% @end
%%% Created :  Mon Jul 09 13:05:58 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(game_dispatch).
%% API
-export([mount/1]).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

mount(Path) ->
  {ok, Mounts} = file:consult(Path),
  lists:foldl(
    fun(Item, Acc) -> Item ++ Acc end,
    [],
    build_dispatches([mount_parts(Item) || Item <- Mounts ])
  ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_dispatches(MountPartsList) ->
  [ build_dispatch(ListPath, SinglePath, ResourceArgs) || {ListPath, SinglePath, ResourceArgs} <- MountPartsList ].

build_dispatch(ListPath, SinglePath, BaseResourceArgs) ->
  ListName = list_to_atom(ListPath),
  SingleName = list_to_atom(SinglePath),
  MoveName = list_to_atom(SinglePath ++ "_move"),
  JoinName = list_to_atom(SinglePath ++ "_join"),
  DispatchNames = [
    {list, atom_to_binary(ListName, latin1)},
    {single, atom_to_binary(SingleName, latin1)},
    {move, atom_to_binary(MoveName, latin1)},
    {join, atom_to_binary(JoinName, latin1)}
  ],
  ResourceArgs = [{paths, DispatchNames} | BaseResourceArgs],
  [ {ListName, [ListPath], games_resource, ResourceArgs },
    {SingleName, [SinglePath, id, player], game_resource, ResourceArgs },
    {MoveName, [SinglePath, id, player, "moves"], game_moves_resource, ResourceArgs },
    {JoinName, [SinglePath, id, "join", player], game_joins_resource, ResourceArgs } ].

mount_parts(MountProps) ->
  RulesModule = get_required_property(rules, MountProps),
  SinglePath = get_property(path, MountProps, fun() -> atom_to_list(RulesModule) end ),
  ListPath = get_property(list_path, MountProps, fun() -> SinglePath ++ "s" end),
  ParlorOpts = get_property(parlor, MountProps, []),
  ResourceOpts = get_property(resources, MountProps, []),
  {
    ListPath,
    SinglePath,
    [{rules_module, RulesModule}, {parlor_opts, ParlorOpts}, {resource_opts, ResourceOpts}]
  }.

get_required_property(Name, Proplist) ->
  case proplists:get_value(Name, Proplist) of
    undefined -> error(missing_property, [Name, Proplist]);
    Value -> Value
  end.

get_property(Name, Proplist, Fun) when is_function(Fun) ->
  try get_required_property(Name, Proplist)
  catch
    error:missing_property -> Fun()
  end;
get_property(Name, Proplist, Default) ->
  get_property(Name, Proplist, fun() -> Default end).
