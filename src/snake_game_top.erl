-module(snake_game_top).
-export([start_link/0, init/1]).
-behavior(supervisor).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok,
    {{one_for_one, 10, 30},
    [ {snake_game_manager,
        {snake_game_manager, start_link, []},
        permanent, 5, worker, [snake_game_manager] },
      {snake_game_soop,
        {snake_game_soop, start_link, []},
        permanent, 5, supervisor, [snake_game_soop, snake_game]}
    ]}}.
