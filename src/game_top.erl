-module(game_top).
-export([start_link/0, init/1]).
-behavior(supervisor).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok,
    {{one_for_one, 10, 30},
    [ {game_manager,
        {game_manager, start_link, []},
        permanent, 5, worker, [game_manager] },
      {game_soop,
        {game_soop, start_link, []},
        permanent, 5, supervisor, [game_soop]}
    ]}}.
