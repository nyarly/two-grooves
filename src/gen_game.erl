-module(gen_game).
-behavior(gen_fsm).

-export([behaviour_info/1]).
%% FSM callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%% Event handlers
%% @todo replace_player
-export([lobby/2, lobby/3, playing/2, playing/3, finished/2, finished/3]).
%% Convenience
-export([start_link/4, start/4, current_state/1, to_proplist/2, join/3, move/3, score/1, quit/1]).

behaviour_info(callbacks) ->
  [{setup, 2}, {join, 3}, {move, 4}, {scores, 2}, {start_game, 2}, {finish_game, 1}, {board_proplist, 3}, {code_change, 3}];
behaviour_info(_) ->
  undefined.

% setup(ParlorOpts, TableOpts) -> {ok, State} / {error, reason}
% join(Player, PlayerOpts, State) -> {ok, NewOpts, State} / {error, reason}
%
% move(Player, PlayerOpts, Move, State) -> State / {error, reason}
% scores(State) -> [{Player, term()}] / no_score
%
% start_game(Players, State) -> {ok, State} / {error, reason}
% finish_game(State) -> {ok, NewState}, {error, not_done}
%
% board_proplist(State, Player) -> proplist()
%
% code_change(Vsn, State) -> State
%
% @todo
% compare_scores(Score, Score) -> better, worse, same
% strength (OldStrength, Score) -> NewStrength

-record(game, {rules, resource_opts, players=[], board}).

init({RulesModule, ParlorOpts, TableOpts, ResourceOpts}) ->
  case RulesModule:setup(ParlorOpts, TableOpts) of
    {ok, State} ->
      {ok,
        lobby,
        #game{
          rules=RulesModule,
          resource_opts=ResourceOpts,
          board=State
        }
      };
    Err = {error, _} -> {stop, Err}
  end.

handle_event(_Event, _StateName, StateData) ->
  {stop, error, StateData}.

handle_sync_event(quit, _StateName, _From, StateData) ->
  {stop, normal, ok, StateData};
handle_sync_event(current_state, StateName, _From, StateData) ->
  {reply, StateName, StateName, StateData};
handle_sync_event(_Event, _StateName, _From, StateData) ->
  {stop, error, error, StateData}.


% @todo Extract internal representation and fix - needs gen_game data, too
lobby({join, Player, PlayerOpts}, _From, State) ->
  case (State#game.rules):join(Player, PlayerOpts, State#game.board) of
    {ok, NewPlayerOpts, NewBoard} ->
      Players = [{Player, NewPlayerOpts} | State#game.players],
      case (State#game.rules):start_game(Players, NewBoard) of
        {ok, StartedBoard} ->
          {reply, {ok, started}, playing, State#game{players=Players, board=StartedBoard}};
        {error, not_ready} ->
          {reply, {ok, joined}, lobby, State#game{players=Players, board=NewBoard}}
      end;
    Err = {error, _} ->
      {reply, Err, lobby, State}
  end;
lobby({proplist, Player}, _From, State) ->
  get_proplist(Player, State);
lobby(score, _From, State) ->
  {reply, {error, not_started}, lobby, State};
lobby({move, _, _}, _From, State) ->
  {reply, {error, bad_state}, lobby, State};
lobby(_, _, State) ->
  {stop, error, error, State}.

lobby(_Request, State) ->
  {next_state, lobby, State}.


playing({move, Player, Move}, _From, State) ->
  case proplists:get_value(Player, State#game.players) of
    undefined -> {reply, {error, not_in_game}, playing, State};
    PlayerOpts ->
      case (State#game.rules):move(Player, PlayerOpts, Move, State#game.board) of
        {ok, NewBoard} ->
          case (State#game.rules):finish_game(NewBoard) of
            {ok, FinishedBoard} ->
              {reply, {ok, finished}, finished, State#game{board=FinishedBoard}};
            {error, not_done} ->
              {reply, {ok, playing}, playing, State#game{board=NewBoard}}
          end;
        Err = {error, _} ->
          {reply, Err, playing, State}
      end
  end;
playing({proplist, Player}, _From, State) ->
  get_proplist(Player, State);
playing(score, _From, State) ->
  get_score(State);
playing({join,_,_}, _From, State) ->
  {reply, bad_state, playing, State};
playing(_, _, State) ->
  {stop, error, error, State}.

playing(_Request, State) ->
  {next_state, playing, State}.


finished({proplist, Player}, _From, State) ->
  get_proplist(Player, State);
finished(score, _From, State) ->
  get_score(State);
finished({move,_,_}, _From, State) ->
  {reply, bad_state, finished, State};
finished({join,_,_}, _From, State) ->
  {reply, bad_state, finished, State};
finished(_, _, State) ->
  {stop, error, error, State}.

finished(_Request, State) ->
  {next_state, finished, State}.


handle_info(_Info, StateName, State) ->
  {noreply, StateName, State}.

terminate(_, _, _) ->
  that_was_fun.

code_change(OldVersion, StateName, State, Extra) ->
  {ok, StateName, (State#game.rules):code_change(OldVersion, State, Extra)}.

%%%%% Helpers %%%%%%%%%%%%%

get_score(State) ->
  case (State#game.rules):scores(proplists:get_keys(State#game.players), State#game.board) of
    {ok, Scores} ->
      {reply, {ok, Scores}, playing, State};
    Err = {error, _} ->
      {reply, Err, playing, State}
  end.

get_proplist(Player, State) ->
  case proplists:get_value(Player, State#game.players) of
    undefined -> {reply, {error, not_in_game}, playing, State};
    PlayerOpts ->
      case (State#game.rules):board_proplist(Player, PlayerOpts, State#game.board) of
        {ok, PropList} -> {reply, build_proplist(State, PropList), lobby, State};
        Err = {error, _} -> {reply, Err, Err, lobby}
      end
  end.

build_proplist(State, GameProplist) ->
  [ { resource_opts, State#game.resource_opts},
    { players, State#game.players }
    | GameProplist ].

%% gen_fsm wrappers

start_link(RulesModule, ParlorOpts, TableOpts, ResourceOpts) ->
  gen_fsm:start_link(?MODULE, {RulesModule, ParlorOpts, TableOpts, ResourceOpts}, []).

start(RulesModule, ParlorOpts, TableOpts, ResourceOpts) ->
  gen_fsm:start(?MODULE, {RulesModule, ParlorOpts, TableOpts, ResourceOpts}, []).

current_state(Game) ->
  gem_fsm:sync_send_all_state_event(Game, current_state).

to_proplist(Game, Player) ->
  gen_fsm:sync_send_event(Game, {proplist, Player}).

join(Game, Player, PlayerOpts) ->
  gem_fsm:sync_send_event(Game, {join, Player, PlayerOpts}).

move(Game, Player, Move) ->
  gen_fsm:sync_send_event(Game, {move, Player, Move}).

score(Game) ->
  gen_fsm:sync_send_event(Game, score).

quit(Game) ->
  gen_fsm:sync_send_all_state_event(Game, quit).
