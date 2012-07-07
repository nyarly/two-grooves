-module(snakegame_rules).
-behavior(gen_game).

-export([setup/2, join/3, start_game/2, move/4, finish_game/1, board_proplist/3, scores/2, code_change/3]).

-record(coords, {x, y}).
-record(board, {moves=[], features=[], dimensions=#coords{x=5,y=5}}).

-define(SIZE_GUARD, X > 0, X =< Board#board.dimensions#coords.x, Y > 0, Y =< Board#board.dimensions#coords.y).

setup(_ParlorOpts, TableOpts) ->
  {X, Y} = proplists:get_value(board_size, TableOpts, {5,5}),
  {ok, #board{
    features=build_features( proplists:get_value(with_features, TableOpts), {X,Y}),
    dimensions=#coords{x=X,y=Y}
  }}.

join(_Player, PlayerOpts, State) ->
  {ok, PlayerOpts, State}.

start_game(Players, State) when length(Players) > 0 ->
  {ok, State};
start_game(_, _) ->
  {error, not_ready}.

move(_Player, _PlayerOpts, MoveProps, State) ->
  {X,Y} = parse_move(MoveProps),
  case do_move(X,Y,State) of
    {ok, NewState} -> {ok, NewState};
    Err = {error, _} -> Err
  end.

finish_game(State=#board{moves=Moves}) when length(Moves) >= 5 ->
  {ok, State};
finish_game(_) ->
  {error, not_done}.

board_proplist(_Player, _PlayerOpts, State) ->
  {ok, [
      {moves, lists:reverse([coord_pair_to_proplist(Pair) || Pair <- State#board.moves])},
      {targets, [target_to_proplist(Target) || Target <- State#board.features]},
      {dims, coord_pair_to_proplist({State#board.dimensions#coords.x, State#board.dimensions#coords.y})},
      {score, score(State)}
    ]}.

scores(Players, State) ->
  {ok, [{hd(Players), score(State)}]}.

code_change(_OldVersion, State, _Extra) ->
  State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

parse_move(Proplist) ->
  {X, []} = string:to_integer(proplists:get_value("x", Proplist)),
  {Y, []} = string:to_integer(proplists:get_value("y", Proplist)),
  {X, Y}.

do_move(X, Y, Board=#board{moves=[]}) when ?SIZE_GUARD ->
  {ok, Board#board{moves=[{X,Y}]}};
do_move(X, Y, Board=#board{moves=Moves=[{X, Yp} | _]}) when ?SIZE_GUARD, abs(Y - Yp) == 1, length(Moves) < 5 ->
  case lists:member({X,Y}, Moves) of
    true ->
      {error, "Can't double back"};
    false ->
      {ok, Board#board{moves=[{X,Y} | Moves]}}
  end;
do_move(X, Y, Board=#board{moves=Moves=[{Xp, Y} | _]}) when ?SIZE_GUARD, abs(X - Xp) == 1, length(Moves) < 5 ->
  case lists:member({X,Y}, Moves) of
    true ->
      {error, "Can't double back"};
    false ->
      {ok, Board#board{moves=[{X,Y} | Moves]}}
  end;
do_move(X, Y, Board) when ?SIZE_GUARD ->
  {error, "Not adjacent to head"};
do_move(_, _, _) ->
  {error, "Out of bounds"}.

score(State) ->
  score(State, 0).

score(#board{features=[]}, Score) ->
  Score;
score(Board=#board{moves=Moves, features=[Thing | Rest]}, Score) ->
  score_item(Moves, Thing) + score(Board#board{features=Rest}, Score).

build_features(undefined, {X,Y}) ->
  random_features(X,Y);
build_features(ExactlyThese, _Dims) ->
  ExactlyThese.

random_features(X,Y) ->
  random_features(X, Y, erlang:now()).

random_features(X, Y, Seed) ->
  {Wiggle, Seed1} = random:uniform_s(max(1,round(X * Y * 0.15)), Seed),
  random_features(X, Y, Seed1, max(1,round(X * Y * 0.15 + Wiggle)), []).

random_features(_X, _Y, _Seed, 0, Acc) ->
  Acc;
random_features(X, Y, Seed, Count, Acc) ->
  {Feature, Seed2} = case random:uniform_s(3, Seed) of
    {1, Seed1} ->
      random_feature(post, X, Y, Seed1);
    {2, Seed1} ->
      random_feature(ns_gate, X, Y, Seed1);
    {3, Seed1} ->
      random_feature(ew_gate, X, Y, Seed1)
  end,
  case lists:member(Feature, Acc) of
    true ->
      random_features(X, Y, Seed2, Count, Acc);
    false ->
      random_features(X, Y, Seed2, Count - 1, [Feature | Acc])
  end.

random_feature(ew_gate, X, Y, Seed) ->
  {FX, Seed1} = random:uniform_s(X - 1, Seed),
  {FY, Seed2} = random:uniform_s(Y, Seed1),
  {{gate, {FX, FY}, {FX + 1, FY}}, Seed2};
random_feature(ns_gate, X, Y, Seed) ->
  {FX, Seed1} = random:uniform_s(X, Seed),
  {FY, Seed2} = random:uniform_s(Y - 1, Seed1),
  {{gate, {FX, FY}, {FX, FY + 1}}, Seed2};
random_feature(post, X, Y, Seed) ->
  {PX, Seed1} = random:uniform_s(X - 1, Seed),
  {PY, Seed2} = random:uniform_s(Y - 1, Seed1),
  {{post, {PX + 1, PY + 1}}, Seed2}.

score_item(Moves, {post, Around}) ->
  score_post(Moves, Around);
score_item(Moves, {gate, From, To}) ->
  score_gate(Moves, From, To);
score_item(_, []) ->
  0.

% A B
% D C - if C is "X Y"
% ergo:
% A = {Ax, Ay} where Ax = X - 1, Ay = Y - 1
% B = {X, By} where By = Y - 1
% C = {X, Y}
% A = {Dx, Y} where Dx = X - 1
% Cases are: ABC BCD CDA DAB
%            CBA DCB ADC BAD

%ABC
score_post([{Ax, Ay}, {X, By}, {X, Y} | _], {X, Y}) when Ax == X - 1, Ay == Y - 1, By == Y - 1 ->
  5;
%BCD
score_post([{X, By}, {X, Y}, {Dx, Y} | _], {X, Y}) when By == Y - 1, Dx == X - 1 ->
  5;
%CDA
score_post([{X, Y}, {Dx, Y}, {Ax, Ay} | _], {X, Y}) when Dx == X - 1, Ax == X - 1, Ay == Y - 1 ->
  5;
%DAB
score_post([{Dx, Y}, {Ax, Ay}, {X, By} | _], {X, Y}) when Ax == X - 1, Ay == Y - 1, Dx == X - 1 , By == Y - 1 ->
  5;
%CBA
score_post([{X, Y}, {X, By}, {Ax, Ay} | _], {X, Y}) when Ax == X - 1, Ay == Y - 1, By == Y - 1 ->
  5;
%DCB
score_post([{Dx, Y}, {X, Y}, {X, By} | _], {X, Y}) when By == Y - 1, Dx == X - 1 ->
  5;
%ADC
score_post([{Ax, Ay}, {Dx, Y}, {X, Y} | _], {X, Y}) when Dx == X - 1, Ax == X - 1, Ay == Y - 1 ->
  5;
%BAD
score_post([{X, By}, {Ax, Ay}, {Dx, Y} | _], {X, Y}) when Ax == X - 1, Ay == Y - 1, Dx == X - 1, By == Y - 1 ->
  5;
score_post([_ | Rest], Around) ->
  score_post(Rest, Around);
score_post([], _) ->
  0.

score_gate([{Fx, Fy}, {Tx, Ty} | _], {Fx, Fy}, {Tx, Ty}) ->
  3;
score_gate([{Tx, Ty}, {Fx, Fy} | _], {Fx, Fy}, {Tx, Ty}) ->
  3;
score_gate([_ | Rest], From, To) ->
  score_gate(Rest, From, To);
score_gate([], _, _) ->
  0.

coord_pair_to_proplist({X, Y}) -> [{x, X}, {y, Y}].

target_to_proplist({post, Around}) ->
  [{type, post}, {around, coord_pair_to_proplist(Around)}];
target_to_proplist({gate, Here, There}) ->
  [{type, gate}, {between, [coord_pair_to_proplist(Here), coord_pair_to_proplist(There)]}].

%% These are the callbacks needed to make a gen_server process out of this - to let the game talk to other processes
