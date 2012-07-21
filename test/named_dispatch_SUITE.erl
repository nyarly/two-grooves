%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Tests for named dispatching
%%% @end
%%% Created :  Sun Jul 15 17:00:24 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(named_dispatch_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("assertions.hrl").

suite() ->
  [{timetrap,{seconds,30}}].
init_per_suite(Config) ->
  [ { target_name, test_template },
    { template, <<"{% path_to name='test' number='7' string='hello' var=var %}">> },
    { compile_opts, [{custom_tags_modules, [two_grooves_custom_tags]}] },
    { render_vars, [{var, "var"}]},
    { dispatches, [
      {test, ["test", number, "name", string, var], test_name_resource, []}
    ]}
    |Config].
end_per_suite(_Config) ->
  ok.
init_per_group(_GroupName, Config) ->
  Config.
end_per_group(_GroupName, _Config) ->
  ok.
init_per_testcase(_TestCase, Config) ->
  Config.
end_per_testcase(_TestCase, _Config) ->
  ok.
groups() ->
  [].
all() ->
  [render_time_dispatch].

render_time_dispatch(Config) ->
  TargetName = proplists:get_value(target_name, Config),
  CompileOpts = proplists:get_value(compile_opts, Config),
  {ok, _} = erlydtl:compile( proplists:get_value(template, Config), TargetName, CompileOpts),
  Vars = proplists:get_value(render_vars, Config),
  RenderConfig = [{custom_tags_context, [{dispatches, two_grooves_named_dispatch:to_proplist(proplists:get_value(dispatches, Config))}]}],
  ct:pal("~p ~p~n", [Vars, RenderConfig]),
  {ok, Rendered} = TargetName:render(Vars,RenderConfig),
  ct:pal("~s~n",[Rendered]),
  <<"/test/7/name/hello/var">> = list_to_binary(Rendered).

%% Been a headache - not needed immediately
compile_time_dispatch(Config) ->
  TargetName = proplists:get_value(target_name, Config),
  CompileOpts = [ {vars, [{dispatches, two_grooves_named_dispatch:to_proplist(proplists:get_value(dispatches, Config))}]} |
    proplists:get_value(compile_opts, Config) ],
  ct:pal("~p~n",[CompileOpts]),
  {ok, _} = erlydtl:compile( proplists:get_value(template, Config), TargetName, CompileOpts),
  {ok, Rendered} = TargetName:render(proplists:get_value(render_vars, Config)),
  <<"/test/7/name/hello">> = list_to_binary(Rendered).
