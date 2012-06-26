-module(two_grooves_static_resource).

-export([init/1, resource_exists/2, encodings_provided/2, content_types_provided/2, serve/2]).
-include_lib("webmachine/include/webmachine.hrl").

-define(CHUNK_SIZE, 4096).

-record(config, {root_dir}).

init([RootDir]) ->
  {{trace, "priv/log"}, #config{root_dir=RootDir}}.

resource_exists(RD, Cfg=#config{root_dir=RootDir}) ->
  Path = filename:join([RootDir, wrq:disp_path(RD)]),
  io:format("~s: found? ~p~n", [Path, filelib:is_file(Path)]),
  {filelib:is_file(Path), RD, Cfg}.

encodings_provided(RD, Cfg) ->
  case wrq:method(RD) of
    'GET' ->
      {[
        {"identity",  fun(X) -> X end},
        {"gzip",      fun gzip/1 }
      ], RD, Cfg};
    _ ->
      {[ {"identity",  fun(X)  ->  X             end} ],
        RD, Cfg}
  end.

gzip(<<>>) ->
  <<>>;
gzip(Content) ->
  io:format("Compressing: ~p~n", [Content]),
  zlib:gzip(Content).

content_types_provided(RD, Cfg) ->
  {[
      {"application/javascript",  serve},
      {"text/html",               serve},
      {"text/css",                serve},
      {"text/plain",              serve},
      {"image/png",               serve},
      {"image/jpeg",              serve},
      {"image/gif",               serve},
      {"image/svg",               serve}
    ], RD, Cfg}.

add_xsendfile(Path, RD) ->
  wrq:set_resp_header("X-Sendfile", filename:nativename(filename:absname(Path)), RD).

stream_next_chunk(Io) ->
  case file:read(Io, ?CHUNK_SIZE) of
    {ok, Data} ->
      io:format("CHUNK: ~p~n", [Data]),
      {Data, fun() -> stream_next_chunk(Io) end};
    eof ->
      io:format("Done~n", []),
      {<<>>, done};
    {error, _} ->
      {<<>>, done}
  end.

serve(RD, Cfg=#config{root_dir=RootDir}) ->
  Path = filename:join([RootDir, wrq:disp_path(RD)]),
  case file:open(Path, [read, binary]) of
    {ok, Io} ->
      {
        {stream, stream_next_chunk(Io)},
        add_xsendfile(Path, RD),
        Cfg
      };
    {error, _} ->
      {
        {halt, 404},
        RD,
        Cfg
      }
  end.
