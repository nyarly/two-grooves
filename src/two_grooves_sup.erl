%% @author Judson Lester <nyarly@gmail.com>
%% @copyright 2012 Judson Lester.

%% @doc Supervisor for the two_grooves application.

-module(two_grooves_sup).
-author('Judson Lester <nyarly@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-export([config_path/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old = sets:from_list(
    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
        supervisor:terminate_child(?MODULE, Id),
        supervisor:delete_child(?MODULE, Id),
        ok
    end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  Ip = case os:getenv("WEBMACHINE_IP") of
    false -> "0.0.0.0";
    Any -> Any
  end,

  {ok, ConfigDispatch} = file:consult(config_path("dispatch.conf")),

  GameDispatch = game_dispatch:mount(config_path("game-dispatch.conf")),

  Dispatch = ConfigDispatch ++ GameDispatch,

  WMDispatch = two_grooves_named_dispatch:wm_dispatches(Dispatch),

  TemplateDispatch = two_grooves_named_dispatch:to_proplist(Dispatch),

  WebConfig = [
    {ip, Ip},
    {port, 8000},
    {log_dir, "priv/log"},
    {dispatch, WMDispatch}],
  Web = {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, [mochiweb_socket_server]},
  Games = {game_top,
    {game_top, start_link, []},
    permanent, 5000, supervisor, [game_top]},
  Processes = [Web, Games],
  {ok, { {one_for_one, 10, 10}, Processes} }.

config_path(FileName) ->
  filename:join( [filename:dirname(code:which(?MODULE)), "..", "priv", FileName]).
