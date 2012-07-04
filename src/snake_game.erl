-module(snake_game).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, start_game/1, see_board/1, make_move/3, get_score/1, quit/1, to_proplist/1]).
-record(coords, {x, y}).
-record(board, {moves=[], features=[], player, dimensions=#coords{x=5,y=5}}).
-define(SIZE_GUARD, X > 0, X =< Board#board.dimensions#coords.x, Y > 0, Y =< Board#board.dimensions#coords.y).

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

setup_board({with_features, Features}) ->
  #board{features=Features};
setup_board({X,Y}) ->
  #board{features=random_features(X,Y), dimensions=#coords{x=X,y=Y}}.

move(X, Y, Board=#board{moves=[]}) when ?SIZE_GUARD ->
  {ok, Board#board{moves=[{X,Y}]}};
move(X, Y, Board=#board{moves=Moves=[{X, Yp} | _]}) when ?SIZE_GUARD, abs(Y - Yp) == 1, length(Moves) < 5 ->
  case lists:member({X,Y}, Moves) of
    true ->
      {error, "Can't double back"};
    false ->
      {ok, Board#board{moves=[{X,Y} | Moves]}}
  end;
move(X, Y, Board=#board{moves=Moves=[{Xp, Y} | _]}) when ?SIZE_GUARD, abs(X - Xp) == 1, length(Moves) < 5 ->
  case lists:member({X,Y}, Moves) of
    true ->
      {error, "Can't double back"};
    false ->
      {ok, Board#board{moves=[{X,Y} | Moves]}}
  end;
move(X, Y, Board) when ?SIZE_GUARD ->
  {error, "Not adjacent to head"};
move(_, _, _) ->
  {error, "Out of bounds"}.

score(State) ->
  score(State, 0).

score(#board{features=[]}, Score) ->
  Score;
score(Board=#board{moves=Moves, features=[Thing | Rest]}, Score) ->
  score_item(Moves, Thing) + score(Board#board{features=Rest}, Score).

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

init(Config) ->
  {ok, setup_board(Config)}.

handle_call(proplist, _From, State) ->
  {reply, {ok,
      [
        {moves, lists:reverse([coord_pair_to_proplist(Pair) || Pair <- State#board.moves])},
        {targets, [target_to_proplist(Target) || Target <- State#board.features]},
        {dims, coord_pair_to_proplist({State#board.dimensions#coords.x, State#board.dimensions#coords.y})},
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
