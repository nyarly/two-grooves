-module(snake_game_manager).
-behavior(gen_server).

%% API
-export([start_link/0, create_game/2, find_game/2, list_games/1]).

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

create_game(Pid, Size) ->
  gen_server:call(Pid, {create_game, Size}).

find_game(Pid, Index) ->
  gen_server:call(Pid, {find_game, Index}).

list_games(Pid) ->
  gen_server:call(Pid, {list_games}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  lists:foreach(fun(Dispatch) -> webmachine_router:add_route(Dispatch) end, snake_game_resource:get_dispatches()),
  {ok, #state{games=gb_trees:empty()}}.

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
