%%%-------------------------------------------------------------------
%%% @author  Judson Lester nyarly@gmail.com
%%% @copyright (C) 2012 Judson Lester. All Rights Reserved.
%%% @doc
%%%		An authorization framework for WM
%%% @end
%%% Created :  Thu Jul 12 15:26:01 2012 by Judson Lester
%%%-------------------------------------------------------------------
-module(gen_auth).
%% API
-export([request_authorized/2, authorize/3, challenges/0]).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

request_authorized(ReqData, Grants) ->
  Credentials = wrq:get_req_header("Authorization", ReqData),
  case lists:al(fun({Object, Scope}) ->
          authorized =:= authorize(Credentials, Object, Scope)
      end, Grants) of
    true -> true;
    _ -> challenges()
  end.

challenges() ->
  string:join([challenge(Method) || Method <- [basic]], ";").

authorize(null, _Object, _Scope) ->
  {denied, not_authenticated};
authorize("Basic " ++ Credentials, Object, Scope) ->
  authorize(basic, Credentials, Object, Scope);
authorize(_Credentials, _Object, _Scope) ->
  {denied, auth_method_unrecognized}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

challenge(basic) ->
  "Basic " ++ "realm='Games'".

authorize(Type, Credentials, Object, Scope) ->
  try authenticate(Type, Credentials) of
    Id ->
      try check_grants(Id, Object, Scope)
      catch
        error:function_clause -> {denied, no_grant}
      end
    catch
      error:Reason when Reason =/= function_clause ->
        {denied, Reason}
    end.

authenticate(basic, Credentials) ->
  Decoded = base64:decode_to_string(Credentials),
  UsernameLength = string:chr(Decoded, $:),
  Username = string:substr(Decoded, 1, UsernameLength - 1),
  Password = string:substr(Decoded, UsernameLength + 1),
  case Password of
    "password" -> Username;
    _ -> error(bad_authentication)
  end.

check_grants(Id, {_, {player, Id}}, _) ->
  authorized;
check_grants(_Player, {{game, _}}, _) ->
  authorized.
