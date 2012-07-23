%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Named dispatch for Web Machine
%%% @end
%%% Created :  Sun Jul 15 15:23:49 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(two_grooves_named_dispatch).
%% API
-export([wm_dispatches/1, to_proplist/1, zip_dispatch/2]).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

wm_dispatches(NamedDispatches) ->
  DispatchProplist = to_proplist(NamedDispatches),
  [dispatch_part(Dispatch, DispatchProplist) || Dispatch <- NamedDispatches].

to_proplist(NamedDispatches) ->
  [{name_part(Dispatch), path_part(Dispatch)} || Dispatch <- NamedDispatches].

zip_dispatch(Path, PathInfo) ->
  [[<<"/">>, Part] || Part <- zip_path(Path, PathInfo, [])].

%%%===================================================================
%%% Internal functions
%%%===================================================================

dispatch_part({_Name, Path, Mod, Opts}, Named) ->
  {Path, Mod, [{dispatches, Named} | Opts]};
dispatch_part({_Name, Path, Guard, Mod, Opts}, Named) ->
  {Path, Guard, Mod, [{dispatches, Named} | Opts]}.

name_part({Name, _,_,_}) ->
  atom_to_binary(Name, latin1);
name_part({Name, _,_,_,_}) ->
  atom_to_binary(Name, latin1).

path_part({_Name, Path, _Mod, _Opts}) ->
  Path;
path_part({_Name, Path, _Guard, _Mod, _Opts}) ->
  Path.

zip_path([], [], Acc) ->
  lists:reverse(Acc);
zip_path([Part | Rest], PathInfo, Acc) when is_atom(Part) ->
  case proplists:get_value(Part, PathInfo) of
    undefined -> error({missing_part, Part});
    Value -> zip_path(Rest, proplists:delete(Part, PathInfo), [erlydtl_filters:format_number(Value) | Acc])
  end;
zip_path([Part | Rest], PathInfo, Acc) ->
  zip_path(Rest, PathInfo, [Part | Acc]).
