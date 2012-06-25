-module(snake_game).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, start_game/1, see_board/1, make_move/3, get_score/1, quit/1, to_proplist/1]).
-define(SIZE_GUARD, X > 0, X =< Wide, Y > 0, Y =< Tall).

setup_board() ->
  {[], [{gate,{3,4}, {3,5}}, {post, {2,2}}], {5,5}}.

move(X, Y, {[], Board, Dims = {Wide, Tall}}) when ?SIZE_GUARD ->
  {ok, {[{X,Y}], Board, Dims}};
move(X, Y, {Moves = [{X, Yp} | _], Board, Dims = {Wide, Tall}}) when ?SIZE_GUARD, abs(Y - Yp) == 1, length(Moves) < 5 ->
  case lists:member({X,Y}, Moves) of
    true ->
      {error, invalid_move};
    false ->
      {ok, {[{X,Y} | Moves], Board, Dims}}
  end;
move(X, Y, {Moves = [{Xp, Y} | _], Board, Dims = {Wide, Tall}}) when ?SIZE_GUARD, abs(X - Xp) == 1, length(Moves) < 5 ->
  case lists:member({X,Y}, Moves) of
    true ->
      {error, invalid_move};
    false ->
      {ok, {[{X,Y} | Moves], Board, Dims}}
  end;
move(_, _, _) ->
  {error, invalid_move}.

score(State) ->
  score(State, 0).

score({_, [], _}, Score) ->
  Score;
score({Moves, [Thing | Board], Dims}, Score) ->
  score_item(Moves, Thing) + score({Moves, Board, Dims}, Score).

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

init(_) ->
  {ok, setup_board()}.

handle_call(proplist, _From, State={Moves, Targets, Dims}) ->
  {reply, {ok,
      [
        {moves, lists:reverse([coord_pair_to_proplist(Pair) || Pair <- Moves])},
        {targets, [target_to_proplist(Target) || Target <- Targets]},
        {dims, coord_pair_to_proplist(Dims)},
        {score, score(State)}
      ]}, State};
handle_call(show, _From, State) ->
  {reply, State, State};
handle_call({move, X, Y}, _From, State) ->
  case move(X,Y,State) of
    {ok, NewState} ->
      {reply, {ok, NewState}, NewState};
    Err = {error, _} ->
      {reply, Err, State}
  end;
handle_call(score, _From, State) ->
  {reply, {ok, score(State)}, State};
handle_call(quit, _From, State) ->
  {stop, normal, ok, State};
handle_call(_, _, _) ->
  {stop, error}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_, _) ->
  that_was_fun.

code_change(_OldVersion, State, _Extra) ->
  State.


%% gen_server wrappers

start_link(Size) ->
  gen_server:start_link(?MODULE, Size, []).

start_game(Size) ->
  gen_server:start(?MODULE, Size, []).

see_board(Board) ->
  gen_server:call(Board, show).

make_move(Board, X, Y) ->
  gen_server:call(Board, {move, X, Y}).

get_score(Board) ->
  gen_server:call(Board, score).

quit(Board) ->
  gen_server:call(Board, quit).

to_proplist(Board) ->
  gen_server:call(Board, proplist).
