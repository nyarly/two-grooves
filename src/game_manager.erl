-module(game_manager).
-behavior(gen_server).

%% API
-export([start_link/0, create_game/3, create_game/4, find_game/1, list_games/0, clear_games/0, next_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {games, max_index=1}).

%%% API functions

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc:  Create a new snake game process
%% @spec:  create_game(Pid, Size::{Wide,High} ) -> ok.
%% @end
%%--------------------------------------------------------------------

create_game(Rules, Parlor, Table) ->
  gen_server:call(?SERVER, {create_game, Rules, Parlor, Table}).

create_game(Id, Rules, Parlor, Table) ->
  gen_server:call(?SERVER, {create_game, Id, Rules, Parlor, Table}).

find_game(Index) ->
  gen_server:call(?SERVER, {find_game, Index}).

list_game_ids() ->
  gen_server:call(?SERVER, {list_game_ids}).

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
  {ok, #state{games=gb_trees:empty()}}.

handle_call({next_id}, _From, State = #state{max_index=Max}) ->
  {reply, {ok, Max}, State#state{max_index=Max + 1}};
handle_call({create_game, NewId, Rules, Parlor, Table}, _From, State = #state{games=Games}) ->
  {ok, NewGame} = game_soop:start_game(Rules, Parlor, Table),
  {reply, {ok, NewGame}, State#state{games=gb_trees:insert(NewId, NewGame, Games)}};
handle_call({create_game, Rules, Parlor, Table}, _From, State = #state{games=Games, max_index=OldMax}) ->
  {ok, NewGame} = game_soop:start_game(Rules, Parlor, Table),
  {reply, {ok, NewGame}, State#state{games=gb_trees:insert(OldMax, NewGame, Games), max_index = OldMax + 1}};
handle_call({find_game, Index}, _From, State = #state{games=Games}) ->
  try gb_trees:get(Index, Games) of
    Game -> {reply, {ok, Game}, State}
  catch
    _:_ ->
      {reply, {error, not_found}, State}
  end;
handle_call({list_game_ids}, _From, State) ->
  {reply, {ok, gb_trees:keys(State#state.games)}, State};
handle_call({list_games}, _From, State) ->
  {reply, {ok, gb_trees:to_list(State#state.games)}, State};
handle_call({reset_games}, _From, State) ->
  ok = loop_games(gb_trees:iterator(State#state.games),
    fun(_Key,Game) -> supervisor:terminate_child(game_soop, Game) end ),
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
