-module(two_grooves_static_resource).

-export([init/1, resource_exists/2, encodings_provided/2, content_types_provided/2, serve/2]).
-include_lib("webmachine/include/webmachine.hrl").

-define(CHUNK_SIZE, 4096).

-record(config, {root_dir}).

init([RootDir]) ->
  {ok, #config{root_dir=RootDir}}.

resource_exists(RD, Cfg) ->
  Path = source_path(RD, Cfg),
  io:format("File: ~s found? ~p~n", [Path, filelib:is_file(Path)]),
  {filelib:is_file(Path), RD, Cfg}.

source_path(RD, #config{root_dir=RootDir}) ->
  filename:join([RootDir, wrq:disp_path(RD)]).

maybe_gzip(RD, Cfg) ->
  case wrq:method(RD) of
    'GET' ->
      case (filelib:file_size(source_path(RD, Cfg)) < ?CHUNK_SIZE) of
        true -> {"gzip", fun gzip/1 };
        false -> no
      end;
    _ -> no
  end.

maybe_deflate(RD, _Cfg) ->
  case wrq:method(RD) of
    'GET' ->
      {"deflate", deflate_fun()};
    _ -> no
  end.

deflate_fun() ->
  ZStream = zlib:open(),
  ok = zlib:deflateInit(ZStream),
  fun(Data) -> deflate(ZStream, Data) end.

deflate(ZStream, <<>>) ->
  Chunk = zlib:deflate(ZStream, <<>>, finish),
  zlib:deflateEnd(ZStream),
  zlib:close(ZStream),
  Chunk;
deflate(ZStream, Data) ->
  zlib:deflate(ZStream, Data).

collect_encodings(RD, Cfg) ->
  lists:foldl(fun(Maybe, List) ->
        case Maybe(RD, Cfg) of
          no -> List;
          Method -> [Method | List]
        end
    end,
    [{"identity", fun(X) -> X end}],
    [fun maybe_gzip/2, fun maybe_deflate/2]).


encodings_provided(RD, Cfg) ->
  {collect_encodings(RD, Cfg), RD, Cfg}.

gzip(<<>>) ->
  <<>>;
gzip(Content) ->
  io:format("Compressing: ~p~n", [Content]),
  zlib:gzip(Content).

content_types_provided(RD, Cfg) ->
  {[
      {"application/javascript",  serve},
      {"application/ecmascript",  serve},
      {"text/html",               serve},
      {"text/css",                serve},
      {"text/plain",              serve},
      {"image/png",               serve},
      {"image/jpeg",              serve},
      {"image/gif",               serve},
      {"image/svg+xml",           serve}
    ], RD, Cfg}.

guess_mime(Ext) ->
  case mochiweb_mime:from_extension(Ext) of
    undefined ->
      guess_mime_from_extension(Ext);
    Mime ->
      Mime
  end.

guess_mime_from_extension(".svg") ->
  "image/svg+xml";
guess_mime_from_extension(_) ->
  "text/html".

add_xsendfile(Path, RD) ->
  wrq:set_resp_header("X-Sendfile", filename:nativename(filename:absname(Path)), RD).

add_content_type(Path, RD) ->
  wrq:set_resp_header("Content-Type", guess_mime(filename:extension(Path)), RD).

stream_next_chunk(Io) ->
  case file:read(Io, ?CHUNK_SIZE) of
    {ok, Data} ->
      {Data, fun() -> stream_next_chunk(Io) end};
    eof ->
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
        add_content_type(Path, add_xsendfile(Path, RD)),
        Cfg
      };
    {error, _} ->
      {
        {halt, 404},
        RD,
        Cfg
      }
  end.
