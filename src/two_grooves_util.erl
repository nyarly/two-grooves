%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		Tools for working with HTTP requests etc.
%%% @end
%%% Created :  Sat Jul 07 17:09:11 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(two_grooves_util).
%% API
-export([decode_query/2]).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

decode_query(NameFuns, Query) ->
  [ parse_value(Name, Fun, Default, Query) ||
    { Name, Fun, Default } <- NameFuns ].

parse_value(Name, Fun, Default, Query) when is_function(Fun) ->
  case proplists:get_value(atom_to_list(Name), Query) of
    undefined -> Default;
    Value -> Fun(Value)
  end;
parse_value(Name, Fun, Default, Query) when is_atom(Fun) ->
  parse_value(Name, parsing_fun(Fun), Default, Query).


parsing_fun(identity) ->
  string;
parsing_fun(string) ->
  fun(X) -> X end;
parsing_fun(integer) ->
  fun(X) -> {Num, []} = string:to_integer(X), Num end;
parsing_fun(atom) ->
  fun(X) -> list_to_atom(X) end.





%%%===================================================================
%%% Internal functions
%%%===================================================================
