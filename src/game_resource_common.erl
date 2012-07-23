-module(game_resource_common).
-export([init/1, build_path/3, render/3]).
-include("include/game_resource.hrl").

init(Config) ->
  [RulesModule, ParlorOpts, ResourceOpts, Paths, Dispatches] =
  [proplists:get_value(Field, Config) || Field <- [rules_module, parlor_opts, resource_opts, paths, dispatches]],
  {ok, #game_context{rules=RulesModule, parlor=ParlorOpts, resource=ResourceOpts, paths=Paths, tag_context=[{dispatches, Dispatches}]}}.

build_path(Name, Context, PathInfo) ->
  Dispatches = proplists:get_value(dispatches, Context#game_context.tag_context),
  Paths = Context#game_context.paths,
  {Name, DispatchName} = proplists:lookup(Name, Paths),
  {DispatchName, Dispatch} = proplists:lookup(DispatchName, Dispatches),
  two_grooves_named_dispatch:zip_dispatch(Dispatch, PathInfo).

render(Template, Data, Context) ->
  {ok, Result} = Template:render(
    [{paths, Context#game_context.paths} | Data],
    [{custom_tags_context, Context#game_context.tag_context}]),
  Result.
