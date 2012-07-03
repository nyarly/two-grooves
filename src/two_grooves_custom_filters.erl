%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Custom filters for Two Grooves
%%% @end
%%% Created :  Sat Jun 30 02:58:50 2012 by Judson Lester
-module(two_grooves_custom_filters).
%% API
-export([is_dictionary/1]).
%%%===================================================================
%%% API
%%%===================================================================

is_dictionary(List) when is_list(List) ->
  is_proplist(List);
is_dictionary(_) ->
  false.

is_array(List) when is_list(List) ->
  not is_dictionary(List) andalso not is_string(List);
is_array(_) ->
  false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_proplist(List) ->
  lists:all(fun(Elem) -> is_atom(Elem) orelse is_tuple(Elem) end, List).

is_string(List) ->
  lists:all(fun(Elem) -> is_integer(Elem) end, List).
