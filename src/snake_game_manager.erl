-module(snake_game_manager).
-behavior(gen_server).

%% API
-export([start_link/0, create_game/1, create_game/2, find_game/1, list_games/0, clear_games/0, next_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {games, max_index=1}).

%%% API functions

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc:  Create a new snake game process
%% @spec:  create_game(Pid, Size::{Wide,High} ) -> ok.
%% @end
%%--------------------------------------------------------------------

create_game(Size) ->
  gen_server:call(?SERVER, {create_game, Size}).

create_game(Id, Size) ->
  gen_server:call(?SERVER, {create_game, Id, Size}).

find_game(Index) ->
  gen_server:call(?SERVER, {find_game, Index}).

list_games() ->
  gen_server:call(?SERVER, {list_games}).

clear_games() ->
  gen_server:call(?SERVER, {reset_games}).

next_id() ->
  gen_server:call(?SERVER, {next_id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  [[webmachine_router:add_route(Dispatch) || Dispatch <- DispatchList] || DispatchList <-
    [ Resource:get_dispatches() || Resource <- [snakegame_resource, snakegames_resource, snakegame_moves_resource]]],

  {ok, #state{games=gb_trees:empty()}}.

handle_call({next_id}, _From, State = #state{max_index=Max}) ->
  {reply, {ok, Max}, State#state{max_index=Max + 1}};
handle_call({create_game, NewId, Size}, _From, State = #state{games=Games}) ->
  {ok, NewGame} = snake_game_soop:start_game(Size),
  {reply, ok, State#state{games=gb_trees:insert(NewId, NewGame, Games)}};
handle_call({create_game, Size}, _From, State = #state{games=Games, max_index=OldMax}) ->
  {ok, NewGame} = snake_game_soop:start_game(Size),
  {reply, ok, State#state{games=gb_trees:insert(OldMax, NewGame, Games), max_index = OldMax + 1}};
handle_call({find_game, Index}, _From, State = #state{games=Games}) ->
  try gb_trees:get(Index, Games) of
    Game -> {reply, {ok, Game}, State}
  catch
    _:_ ->
      {reply, {error, not_found}, State}
  end;
handle_call({list_games}, _From, State) ->
  {reply, {ok, gb_trees:keys(State#state.games)}, State};
handle_call({reset_games}, _From, State) ->
  ok = loop_games(gb_trees:iterator(State#state.games),
    fun(_Key,Game) -> supervisor:terminate_child(snake_game_soop, Game) end ),
  {reply, ok, State#state{games=gb_trees:empty()}};
handle_call(_, _, State) ->
  {reply, {error, bad_message}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop_games(Iter, Fun) ->
  case gb_trees:next(Iter) of
    none ->
      ok;
    {Key, Val, Iter2} ->
      Fun(Key, Val),
      loop_games(Iter2, Fun)
  end.
