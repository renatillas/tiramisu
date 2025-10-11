-module(mist@internal@file).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/mist/internal/file.gleam").
-export([sendfile/6, open/1, stat/1, close/1]).
-export_type([file_descriptor/0, file_error/0, send_error/0, file/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type file_descriptor() :: any().

-type file_error() :: is_dir | no_access | no_entry | unknown_file_error.

-type send_error() :: {file_err, file_error()} |
    {socket_err, glisten@socket:socket_reason()}.

-type file() :: {file, file_descriptor(), integer()}.

-file("src/mist/internal/file.gleam", 33).
?DOC(false).
-spec sendfile(
    glisten@transport:transport(),
    file_descriptor(),
    glisten@socket:socket(),
    integer(),
    integer(),
    list(any())
) -> {ok, nil} | {error, send_error()}.
sendfile(Transport, File_descriptor, Socket, Offset, Bytes, Options) ->
    case Transport of
        tcp ->
            _pipe = file:sendfile(
                File_descriptor,
                Socket,
                Offset,
                Bytes,
                Options
            ),
            _pipe@1 = gleam@result:map_error(
                _pipe,
                fun(Field@0) -> {socket_err, Field@0} end
            ),
            gleam@result:replace(_pipe@1, nil);

        ssl = Transport@1 ->
            _pipe@2 = file:pread(File_descriptor, Offset, Bytes),
            _pipe@3 = gleam@result:map_error(
                _pipe@2,
                fun(Field@0) -> {file_err, Field@0} end
            ),
            gleam@result:'try'(
                _pipe@3,
                fun(Bits) ->
                    _pipe@4 = glisten@transport:send(
                        Transport@1,
                        Socket,
                        gleam@bytes_tree:from_bit_array(Bits)
                    ),
                    gleam@result:map_error(
                        _pipe@4,
                        fun(Field@0) -> {socket_err, Field@0} end
                    )
                end
            )
    end.

-file("src/mist/internal/file.gleam", 75).
?DOC(false).
-spec open(bitstring()) -> {ok, file_descriptor()} | {error, file_error()}.
open(File) ->
    mist_ffi:file_open(File).

-file("src/mist/internal/file.gleam", 24).
?DOC(false).
-spec stat(bitstring()) -> {ok, file()} | {error, file_error()}.
stat(Filename) ->
    _pipe = Filename,
    _pipe@1 = mist_ffi:file_open(_pipe),
    gleam@result:map(
        _pipe@1,
        fun(Fd) ->
            File_size = filelib:file_size(Filename),
            {file, Fd, File_size}
        end
    ).

-file("src/mist/internal/file.gleam", 81).
?DOC(false).
-spec close(file_descriptor()) -> {ok, nil} | {error, file_error()}.
close(File) ->
    mist_ffi:file_close(File).
