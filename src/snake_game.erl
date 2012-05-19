-module(snake_game).
-export([init/0, move/3, score/1]).

init() ->
  {[], [{gate,{3,4}, {3,5}}, {post, {2,2}}]}.

move(X, Y, {[], Board}) ->
  {[{X,Y}], Board};
move(X, Y, {Moves = [{X, Yp} | Rest], Board}) when abs(Y - Yp) == 1, length(Moves) < 5 ->
  {[{X,Y}], Board};
move(X, Y, {Moves = [{Xp, Y} | Rest], Board}) when abs(X - Xp) == 1, length(Moves) < 5 ->
  {[{X,Y}], Board};
move(X, Y, {Moves, Board}) ->
  {[{X,Y} | Moves], Board}.

score(State) ->
  score(State, 0).

score({_, []}, Score) ->
  Score;
score({Moves, [Thing | Board]}, Score) ->
  score_item(Moves, Thing) + score({Moves, Board}, Score).

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
score_post([{Ax, Ay}, {X, By}, {X, Y} | _], {X, Y}) when Ax == (X - 1), Ay == (Y - 1), By == (Y - 1) ->
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
