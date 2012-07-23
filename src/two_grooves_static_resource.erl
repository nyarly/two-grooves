-module(two_grooves_static_resource).

-export([init/1, resource_exists/2, encodings_provided/2, content_types_provided/2, last_modified/2, generate_etag/2, serve/2]).
-include_lib("webmachine/include/webmachine.hrl").

-define(CHUNK_SIZE, 4096).

-record(config, {root_dir, zstream}).

%%Resource API functions

init(Config) ->
  {root_dir, RootDir} = proplists:lookup(root_dir, Config),
  {ok, #config{root_dir=RootDir}}.

resource_exists(RD, Cfg) ->
  Path = source_path(RD, Cfg),
  {filelib:is_file(Path), RD, Cfg}.

encodings_provided(RD, Cfg) ->
  {Cfg2, Encodings} = collect_encodings(RD, Cfg),
  {Encodings, RD, Cfg2}.

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

last_modified(RD, Cfg) ->
  Path = source_path(RD, Cfg),
  case filelib:is_file(Path) of
    true -> { filelib:last_modified(Path), RD, Cfg };
    _ -> {undefined, RD, Cfg}
  end.

generate_etag(RD, Cfg) ->
  Path = source_path(RD, Cfg),
  case filelib:is_file(Path) of
    true ->
      {{Y, Mo, D},{H, Mn, S}} = filelib:last_modified(Path),
      { io_lib:format("W/~w~w~w~w~w~w", [Y,Mo,D,H,Mn,S]), RD, Cfg };
    _ -> {undefined, RD, Cfg}
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

%%Internal functions

source_path(RD, #config{root_dir=RootDir}) ->
  filename:join([RootDir, wrq:disp_path(RD)]).

maybe_gzip(RD, Cfg) ->
  case wrq:method(RD) of
    'GET' ->
      case (filelib:file_size(source_path(RD, Cfg)) < ?CHUNK_SIZE) of
        true -> {Cfg, {"gzip", fun gzip/1 }};
        false -> no
      end;
    _ -> no
  end.

maybe_deflate(RD, Cfg) ->
  case wrq:method(RD) of
    'GET' ->
      case Cfg#config.zstream of
        undefined ->
          ZStreamRef = make_ref(),
          Fun = fun(Data) -> deflate(ZStreamRef, Data) end,
          {Cfg#config{zstream=ZStreamRef}, {"deflate", Fun}};
        ZStreamRef ->
          Fun = fun(Data) -> deflate(ZStreamRef, Data) end,
          {Cfg, {"deflate", Fun}}
      end;
    _ -> no
  end.

retrieve_zstream(ZStreamRef) ->
  case get(ZStreamRef) of
    undefined ->
      ZStream = zlib:open(),
      ok = zlib:deflateInit(ZStream),
      put(ZStreamRef, ZStream),
      ZStream;
    ZStream ->
      ZStream
  end.

deflate(ZStreamRef, <<>>) ->
  ZStream = retrieve_zstream(ZStreamRef),
  Chunk = zlib:deflate(ZStream, <<>>, finish),
  zlib:deflateEnd(ZStream),
  zlib:close(ZStream),
  put(ZStreamRef, undefined),
  Chunk;
deflate(ZStreamRef, Data) ->
  zlib:deflate(retrieve_zstream(ZStreamRef), Data).

collect_encodings(RD, Cfg) ->
  lists:foldl(fun(Maybe, {Ctx, List}) ->
        case Maybe(RD, Cfg) of
          no -> {Ctx, List};
          {Ctx2, Method} -> {Ctx2, [Method | List]}
        end
    end,
    {Cfg, [{"identity", fun(X) -> X end}]},
    [fun maybe_gzip/2, fun maybe_deflate/2]).

gzip(<<>>) ->
  <<>>;
gzip(Content) ->
  zlib:gzip(Content).

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
