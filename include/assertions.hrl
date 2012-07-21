%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson

%% Including this file turns on testing and defines TEST, unless NOTEST
%% is defined before the file is included. If both NOTEST and TEST are
%% already defined, then TEST takes precedence, and NOTEST will become
%% undefined.
%%
%% If NODEBUG is defined before this file is included, the debug macros
%% are disabled, unless DEBUG is also defined, in which case NODEBUG
%% will become undefined. NODEBUG also implies NOASSERT, unless testing
%% is enabled.
%%
%% If including this file causes TEST to be defined, then NOASSERT will
%% be undefined, even if it was previously defined and even if NODEBUG
%% is defined. If both ASSERT and NOASSERT are defined before the file
%% is included, then ASSERT takes precedence, and NOASSERT will become
%% undefined regardless of TEST.
%%
%% After including this file, EUNIT will be defined if and only if TEST
%% is defined.

-ifndef(ASSERTIONS).
-define(ASSERTIONS, true).

%% This macro yields 'true' if the value of E matches the guarded
%% pattern G, otherwise 'false'.
-ifndef(MATCHES).
-define(MATCHES(G,E), (case (E) of G -> true; _ -> false end)).
-endif.

%% The assert macro is written the way it is so as not to cause warnings
%% for clauses that cannot match, even if the expression is a constant.
-undef(assert).
-define(assert(BoolExpr),
  ((fun () ->
          case (BoolExpr) of
            true -> ok;
            __V -> .erlang:error({assertion_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??BoolExpr)},
                  {expected, true},
                  {value, case __V of false -> __V;
                      _ -> {not_a_boolean,__V}
                    end}]})
        end
    end)())).
-define(assertNot(BoolExpr), ?assert(not (BoolExpr))).

-define(assertFun(Fun, Val),
  ((fun () ->
          case (Fun(Val)) of
            true -> ok;
            __V -> .erlang:error({assertion_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Fun)},
                  {result, __V},
                  {value, Val}
                ]})
        end
    end)())).
-define(assertNotFun(Fun, Val),
  ((fun () ->
          case (Fun(Val)) of
            true -> .erlang:error({assertion_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Fun)},
                  {result, __V},
                  {value, Val}
                ]});
            __V -> ok
        end
    end)())).

%% This is mostly a convenience which gives more detailed reports.
%% Note: Guard is a guarded pattern, and can not be used for value.
-define(assertMatch(Guard, Expr),
  ((fun () ->
          case (Expr) of
            Guard -> ok;
            __V -> .erlang:error({assertMatch_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Expr)},
                  {pattern, (??Guard)},
                  {value, __V}]})
        end
    end)())).

%% This is the inverse case of assertMatch, for convenience.
-define(assertNotMatch(Guard, Expr),
  ((fun () ->
          __V = (Expr),
          case __V of
            Guard -> .erlang:error({assertNotMatch_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Expr)},
                  {pattern, (??Guard)},
                  {value, __V}]});
          _ -> ok
        end
    end)())).

%% This is a convenience macro which gives more detailed reports when
%% the expected LHS value is not a pattern, but a computed value
-define(assertEqual(Expect, Expr),
  ((fun (__X) ->
          case (Expr) of
            __X -> ok;
            __V -> .erlang:error({assertEqual_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Expr)},
                  {expected, __X},
                  {value, __V}]})
        end
    end)(Expect))).

%% This is the inverse case of assertEqual, for convenience.
-define(assertNotEqual(Unexpected, Expr),
  ((fun (__X) ->
          case (Expr) of
            __X -> .erlang:error({assertNotEqual_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Expr)},
                  {value, __X}]});
          _ -> ok
        end
    end)(Unexpected))).

%% Note: Class and Term are patterns, and can not be used for value.
%% Term can be a guarded pattern, but Class cannot.
-define(assertException(Class, Term, Expr),
  ((fun () ->
          try (Expr) of
            __V -> .erlang:error({assertException_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Expr)},
                  {pattern,
                    "{ "++(??Class)++" , "++(??Term)
                    ++" , [...] }"},
                  {unexpected_success, __V}]})
          catch
            Class:Term -> ok;
            __C:__T ->
              .erlang:error({assertException_failed,
                [{module, ?MODULE},
                  {line, ?LINE},
                  {expression, (??Expr)},
                  {pattern,
                    "{ "++(??Class)++" , "++(??Term)
                    ++" , [...] }"},
                  {unexpected_exception,
                    {__C, __T,
                      .erlang:get_stacktrace()}}]})
      end
  end)())).

-define(assertError(Term, Expr), ?assertException(error, Term, Expr)).
-define(assertExit(Term, Expr), ?assertException(exit, Term, Expr)).
-define(assertThrow(Term, Expr), ?assertException(throw, Term, Expr)).

%% This is the inverse case of assertException, for convenience.
%% Note: Class and Term are patterns, and can not be used for value.
%% Both Class and Term can be guarded patterns.
-define(assertNotException(Class, Term, Expr),
  ((fun () ->
          try (Expr) of
            _ -> ok
            catch
              __C:__T ->
                case __C of
                  Class ->
                    case __T of
                      Term ->
                        .erlang:error({assertNotException_failed,
                          [{module, ?MODULE},
                            {line, ?LINE},
                            {expression, (??Expr)},
                            {pattern,
                              "{ "++(??Class)++" , "
                              ++(??Term)++" , [...] }"},
                            {unexpected_exception,
                              {__C, __T,
                                .erlang:get_stacktrace()
                            }}]});
                  _ -> ok
                end;
              _ -> ok
            end
        end
    end)())).

-define(debugMsg(S),
  (begin
        .io:fwrite(user, <<"~s:~w:~w: ~s\n">>,
        [?FILE, ?LINE, self(), S]),
      ok
  end)).
-define(debugHere, (?debugMsg("<-"))).
-define(debugFmt(S, As), (?debugMsg(.io_lib:format((S), (As))))).
-define(debugVal(E),
  ((fun (__V) ->
          ?debugFmt(<<"~s = ~P">>, [(??E), __V, 15]),
          __V
      end)(E))).
-define(debugTime(S, E),
  ((fun () ->
          {__T0, _} = statistics(wall_clock),
          __V = (E),
          {__T1, _} = statistics(wall_clock),
          ?debugFmt(<<"~s: ~.3f s">>, [(S), (__T1-__T0)/1000]),
          __V
      end)())).

-endif.
