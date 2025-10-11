-module(simplifile).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/simplifile.gleam").
-export([describe_error/1, file_info_permissions_octal/1, file_info_type/1, file_info/1, link_info/1, delete/1, delete_all/1, read_bits/1, read/1, write_bits/2, write/2, append_bits/2, append/2, is_directory/1, create_directory/1, create_symlink/2, create_link/2, read_directory/1, is_file/1, is_symlink/1, create_file/1, create_directory_all/1, copy_file/2, rename_file/2, rename/2, copy_directory/2, copy/2, rename_directory/2, clear_directory/1, get_files/1, file_permissions_to_octal/1, file_info_permissions/1, set_permissions_octal/2, set_permissions/2, current_directory/0]).
-export_type([file_error/0, file_info/0, file_type/0, permission/0, file_permissions/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type file_error() :: eacces |
    eagain |
    ebadf |
    ebadmsg |
    ebusy |
    edeadlk |
    edeadlock |
    edquot |
    eexist |
    efault |
    efbig |
    eftype |
    eintr |
    einval |
    eio |
    eisdir |
    eloop |
    emfile |
    emlink |
    emultihop |
    enametoolong |
    enfile |
    enobufs |
    enodev |
    enolck |
    enolink |
    enoent |
    enomem |
    enospc |
    enosr |
    enostr |
    enosys |
    enotblk |
    enotdir |
    enotsup |
    enxio |
    eopnotsupp |
    eoverflow |
    eperm |
    epipe |
    erange |
    erofs |
    espipe |
    esrch |
    estale |
    etxtbsy |
    exdev |
    not_utf8 |
    {unknown, binary()}.

-type file_info() :: {file_info,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-type file_type() :: file | directory | symlink | other.

-type permission() :: read | write | execute.

-type file_permissions() :: {file_permissions,
        gleam@set:set(permission()),
        gleam@set:set(permission()),
        gleam@set:set(permission())}.

-file("src/simplifile.gleam", 124).
?DOC(
    " Convert an error into a human-readable description\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert \"Input/output error\" = describe_error(Eio)\n"
    " ```\n"
).
-spec describe_error(file_error()) -> binary().
describe_error(Error) ->
    case Error of
        eperm ->
            <<"Operation not permitted"/utf8>>;

        enoent ->
            <<"No such file or directory"/utf8>>;

        esrch ->
            <<"No such process"/utf8>>;

        eintr ->
            <<"Interrupted system call"/utf8>>;

        eio ->
            <<"Input/output error"/utf8>>;

        enxio ->
            <<"Device not configured"/utf8>>;

        ebadf ->
            <<"Bad file descriptor"/utf8>>;

        edeadlk ->
            <<"Resource deadlock avoided"/utf8>>;

        edeadlock ->
            <<"Resource deadlock avoided"/utf8>>;

        enomem ->
            <<"Cannot allocate memory"/utf8>>;

        eacces ->
            <<"Permission denied"/utf8>>;

        efault ->
            <<"Bad address"/utf8>>;

        enotblk ->
            <<"Block device required"/utf8>>;

        ebusy ->
            <<"Resource busy"/utf8>>;

        eexist ->
            <<"File exists"/utf8>>;

        exdev ->
            <<"Cross-device link"/utf8>>;

        enodev ->
            <<"Operation not supported by device"/utf8>>;

        enotdir ->
            <<"Not a directory"/utf8>>;

        eisdir ->
            <<"Is a directory"/utf8>>;

        einval ->
            <<"Invalid argument"/utf8>>;

        enfile ->
            <<"Too many open files in system"/utf8>>;

        emfile ->
            <<"Too many open files"/utf8>>;

        etxtbsy ->
            <<"Text file busy"/utf8>>;

        efbig ->
            <<"File too large"/utf8>>;

        enospc ->
            <<"No space left on device"/utf8>>;

        espipe ->
            <<"Illegal seek"/utf8>>;

        erofs ->
            <<"Read-only file system"/utf8>>;

        emlink ->
            <<"Too many links"/utf8>>;

        epipe ->
            <<"Broken pipe"/utf8>>;

        erange ->
            <<"Result too large"/utf8>>;

        eagain ->
            <<"Resource temporarily unavailable"/utf8>>;

        enotsup ->
            <<"Operation not supported"/utf8>>;

        enobufs ->
            <<"No buffer space available"/utf8>>;

        eloop ->
            <<"Too many levels of symbolic links"/utf8>>;

        enametoolong ->
            <<"File name too long"/utf8>>;

        edquot ->
            <<"Disc quota exceeded"/utf8>>;

        estale ->
            <<"Stale NFS file handle"/utf8>>;

        enolck ->
            <<"No locks available"/utf8>>;

        enosys ->
            <<"Function not implemented"/utf8>>;

        eftype ->
            <<"Inappropriate file type or format"/utf8>>;

        eoverflow ->
            <<"Value too large to be stored in data type"/utf8>>;

        ebadmsg ->
            <<"Bad message"/utf8>>;

        emultihop ->
            <<"Multihop attempted"/utf8>>;

        enolink ->
            <<"Link has been severed"/utf8>>;

        enosr ->
            <<"No STREAM resources"/utf8>>;

        enostr ->
            <<"Not a STREAM"/utf8>>;

        eopnotsupp ->
            <<"Operation not supported on socket"/utf8>>;

        not_utf8 ->
            <<"File not UTF-8 encoded"/utf8>>;

        {unknown, Inner} ->
            <<"Unknown error: "/utf8, Inner/binary>>
    end.

-file("src/simplifile.gleam", 223).
?DOC(
    " Extract the file permissions from a given FileInfo value in their octal representation.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " use info <- result.try(simplifile.file_info(\"src/app.gleam\"))\n"
    " simplifile.file_info_permissions_octal(info)\n"
    " // --> 0o644\n"
    " ```\n"
).
-spec file_info_permissions_octal(file_info()) -> integer().
file_info_permissions_octal(File_info) ->
    erlang:'band'(erlang:element(3, File_info), 8#777).

-file("src/simplifile.gleam", 252).
?DOC(
    " Extract the file type from a given FileInfo value.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " use info <- result.try(simplifile.file_info(\"src/app.gleam\"))\n"
    " simplifile.file_info_type(info)\n"
    " // --> simplifile.File\n"
    " ```\n"
).
-spec file_info_type(file_info()) -> file_type().
file_info_type(File_info) ->
    case erlang:'band'(erlang:element(3, File_info), 8#170000) of
        8#100000 ->
            file;

        8#40000 ->
            directory;

        8#120000 ->
            symlink;

        _ ->
            other
    end.

-file("src/simplifile.gleam", 272).
?DOC(
    " Get information about a file at a given path\n"
    "\n"
    " When the given `filepath` points to a symlink, this function will follow\n"
    " the symlink and return information about the target file.\n"
    "\n"
    " See `link_info` if you want to get information about a symlink instead.\n"
).
-spec file_info(binary()) -> {ok, file_info()} | {error, file_error()}.
file_info(Filepath) ->
    simplifile_erl:file_info(Filepath).

-file("src/simplifile.gleam", 282).
?DOC(
    " Get information about a file at a given path\n"
    "\n"
    " When the given `filepath` is a symlink, this function will return\n"
    " infromation about the symlink itself.\n"
    "\n"
    " See `file_info` if you want to follow symlinks instead.\n"
).
-spec link_info(binary()) -> {ok, file_info()} | {error, file_error()}.
link_info(Filepath) ->
    simplifile_erl:link_info(Filepath).

-file("src/simplifile.gleam", 326).
?DOC(
    " Delete a file or directory at a given path. Performs a recursive\n"
    " delete on a directory.\n"
    " Throws an error if the path does not exist.\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(Nil) = delete(file_at: \"./delete_me.txt\")\n"
    " ```\n"
).
-spec delete(binary()) -> {ok, nil} | {error, file_error()}.
delete(Path) ->
    simplifile_erl:delete(Path).

-file("src/simplifile.gleam", 333).
?DOC(
    " Delete all files/directories specified in a list of paths.\n"
    " Recursively deletes provided directories.\n"
    " Does not return an error if one or more of the provided paths\n"
    " do not exist.\n"
).
-spec delete_all(list(binary())) -> {ok, nil} | {error, file_error()}.
delete_all(Paths) ->
    case Paths of
        [] ->
            {ok, nil};

        [Path | Rest] ->
            case simplifile_erl:delete(Path) of
                {ok, nil} ->
                    delete_all(Rest);

                {error, enoent} ->
                    delete_all(Rest);

                E ->
                    E
            end
    end.

-file("src/simplifile.gleam", 367).
?DOC(
    " Read a files contents as a bitstring\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(records) = read_bits(from: \"./users.csv\")\n"
    " ```\n"
).
-spec read_bits(binary()) -> {ok, bitstring()} | {error, file_error()}.
read_bits(Filepath) ->
    simplifile_erl:read_bits(Filepath).

-file("src/simplifile.gleam", 290).
?DOC(
    " Read a files contents as a string\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(records) = read(from: \"./users.csv\")\n"
    " ```\n"
).
-spec read(binary()) -> {ok, binary()} | {error, file_error()}.
read(Filepath) ->
    case simplifile_erl:read_bits(Filepath) of
        {ok, Bits} ->
            case gleam@bit_array:to_string(Bits) of
                {ok, Str} ->
                    {ok, Str};

                _ ->
                    {error, not_utf8}
            end;

        {error, E} ->
            {error, E}
    end.

-file("src/simplifile.gleam", 377).
?DOC(
    " Write a bitstring to a file at the given path\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(Nil) = write_bits(to: \"./hello_world.txt\", bits: <<\"Hello, World!\":utf8>>)\n"
    " ```\n"
).
-spec write_bits(binary(), bitstring()) -> {ok, nil} | {error, file_error()}.
write_bits(Filepath, Bits) ->
    simplifile_erl:write_bits(Filepath, Bits).

-file("src/simplifile.gleam", 308).
?DOC(
    " Write a string to a file at the given path\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(Nil) = write(to: \"./hello_world.txt\", contents: \"Hello, World!\")\n"
    " ```\n"
).
-spec write(binary(), binary()) -> {ok, nil} | {error, file_error()}.
write(Filepath, Contents) ->
    _pipe = Contents,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    simplifile_erl:write_bits(Filepath, _pipe@1).

-file("src/simplifile.gleam", 390).
?DOC(
    " Append a bitstring to the contents of a file at the given path\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(Nil) = append_bits(to: \"./needs_more_text.txt\", bits: <<\"more text\":utf8>>)\n"
    " ```\n"
).
-spec append_bits(binary(), bitstring()) -> {ok, nil} | {error, file_error()}.
append_bits(Filepath, Bits) ->
    simplifile_erl:append_bits(Filepath, Bits).

-file("src/simplifile.gleam", 351).
?DOC(
    " Append a string to the contents of a file at the given path\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(Nil) = append(to: \"./needs_more_text.txt\", contents: \"more text\")\n"
    " ```\n"
).
-spec append(binary(), binary()) -> {ok, nil} | {error, file_error()}.
append(Filepath, Contents) ->
    _pipe = Contents,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    simplifile_erl:append_bits(Filepath, _pipe@1).

-file("src/simplifile.gleam", 404).
?DOC(
    " Checks if the provided filepath exists and is a directory.\n"
    " Returns an error if it lacks permissions to read the directory.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(True) = is_directory(\"./test\")\n"
    " ```\n"
).
-spec is_directory(binary()) -> {ok, boolean()} | {error, file_error()}.
is_directory(Filepath) ->
    simplifile_erl:is_directory(Filepath).

-file("src/simplifile.gleam", 415).
?DOC(
    " Create a directory at the provided filepath. Returns an error if\n"
    " the directory already exists.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " create_directory(\"./test\")\n"
    " ```\n"
).
-spec create_directory(binary()) -> {ok, nil} | {error, file_error()}.
create_directory(Filepath) ->
    simplifile_erl:create_directory(Filepath).

-file("src/simplifile.gleam", 430).
?DOC(
    " Create a symbolic link called symlink pointing to target.\n"
    " \n"
    " ### Footgun Alert \n"
    " the target path is relative to *the symlink*,\n"
    " not the current working directory. I will likely be updating \n"
    " the label on the next major version to reflect that.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " create_symlink(\"../target\", \"./symlink\")\n"
    " ```\n"
).
-spec create_symlink(binary(), binary()) -> {ok, nil} | {error, file_error()}.
create_symlink(Target, Symlink) ->
    simplifile_erl:create_symlink(Target, Symlink).

-file("src/simplifile.gleam", 445).
?DOC(
    " Create a \"hard link\" called symlink pointing to target.\n"
    " This does not have the same relative pathing footgun as \n"
    " `create_symlink`.\n"
    " \n"
    " ## Example\n"
    " ```gleam\n"
    " create_link(\"../target\", \"./link\")\n"
    " ```\n"
).
-spec create_link(binary(), binary()) -> {ok, nil} | {error, file_error()}.
create_link(Target, Link) ->
    simplifile_erl:create_link(Target, Link).

-file("src/simplifile.gleam", 460).
?DOC(
    " Lists the contents of a directory.\n"
    " The list contains directory and file names, and is not recursive.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(files_and_folders) = read_directory(at: \"./Folder1\")\n"
    " ```\n"
).
-spec read_directory(binary()) -> {ok, list(binary())} | {error, file_error()}.
read_directory(Path) ->
    simplifile_erl:read_directory(Path).

-file("src/simplifile.gleam", 472).
?DOC(
    " Checks if the file at the provided filepath exists and is a file.\n"
    " Returns an Error if it lacks permissions to read the file.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(True) = is_file(\"./test.txt\")\n"
    " ```\n"
).
-spec is_file(binary()) -> {ok, boolean()} | {error, file_error()}.
is_file(Filepath) ->
    simplifile_erl:is_file(Filepath).

-file("src/simplifile.gleam", 484).
?DOC(
    " Checks if the file at the provided filepath exists and is a symbolic link.\n"
    " Returns an Error if it lacks permissions to read the file.\n"
    "\n"
    " ## Example\n"
    " ```gleam\n"
    " let assert Ok(True) = is_symlink(\"./symlink\")\n"
    " ```\n"
).
-spec is_symlink(binary()) -> {ok, boolean()} | {error, file_error()}.
is_symlink(Filepath) ->
    simplifile_erl:is_symlink(Filepath).

-file("src/simplifile.gleam", 489).
?DOC(
    " Creates an empty file at the given filepath. Returns an `Error(Eexist)`\n"
    " if the file already exists.\n"
).
-spec create_file(binary()) -> {ok, nil} | {error, file_error()}.
create_file(Filepath) ->
    case {begin
            _pipe = Filepath,
            simplifile_erl:is_file(_pipe)
        end,
        begin
            _pipe@1 = Filepath,
            simplifile_erl:is_directory(_pipe@1)
        end} of
        {{ok, true}, _} ->
            {error, eexist};

        {_, {ok, true}} ->
            {error, eexist};

        {_, _} ->
            simplifile_erl:write_bits(Filepath, <<>>)
    end.

-file("src/simplifile.gleam", 500).
?DOC(
    " Recursively creates necessary directories for a given directory\n"
    " path. Note that if you pass a path that \"looks like\" a file, i.e.\n"
    " `./a/b.txt`, a folder named `b.txt` will be created, so be sure\n"
    " to pass only the path to the required directory.\n"
).
-spec create_directory_all(binary()) -> {ok, nil} | {error, file_error()}.
create_directory_all(Dirpath) ->
    Is_abs = filepath:is_absolute(Dirpath),
    Path = begin
        _pipe = Dirpath,
        _pipe@1 = filepath:split(_pipe),
        gleam@list:fold(_pipe@1, <<""/utf8>>, fun filepath:join/2)
    end,
    Path@1 = case Is_abs of
        true ->
            <<"/"/utf8, Path/binary>>;

        false ->
            Path
    end,
    simplifile_erl:create_dir_all(<<Path@1/binary, "/"/utf8>>).

-file("src/simplifile.gleam", 539).
?DOC(
    " Copy a file at a given path to another path.\n"
    " Note: destination should include the filename, not just the directory\n"
).
-spec copy_file(binary(), binary()) -> {ok, nil} | {error, file_error()}.
copy_file(Src, Dest) ->
    _pipe = file:copy(Src, Dest),
    gleam@result:replace(_pipe, nil).

-file("src/simplifile.gleam", 553).
?DOC(
    " Rename a file at a given path to another path.\n"
    " Note: destination should include the filename, not just the directory\n"
).
-spec rename_file(binary(), binary()) -> {ok, nil} | {error, file_error()}.
rename_file(Src, Dest) ->
    simplifile_erl:rename_file(Src, Dest).

-file("src/simplifile.gleam", 558).
?DOC(" Rename a file or directory.\n").
-spec rename(binary(), binary()) -> {ok, nil} | {error, file_error()}.
rename(Src, Dest) ->
    simplifile_erl:rename_file(Src, Dest).

-file("src/simplifile.gleam", 569).
-spec do_copy_directory(binary(), binary()) -> {ok, nil} | {error, file_error()}.
do_copy_directory(Src, Dest) ->
    gleam@result:'try'(
        simplifile_erl:read_directory(Src),
        fun(Segments) ->
            _pipe = Segments,
            gleam@list:each(
                _pipe,
                fun(Segment) ->
                    Src_path = filepath:join(Src, Segment),
                    Dest_path = filepath:join(Dest, Segment),
                    gleam@result:'try'(
                        simplifile_erl:file_info(Src_path),
                        fun(Src_info) -> case file_info_type(Src_info) of
                                file ->
                                    gleam@result:'try'(
                                        simplifile_erl:read_bits(Src_path),
                                        fun(Content) -> _pipe@1 = Content,
                                            simplifile_erl:write_bits(
                                                Dest_path,
                                                _pipe@1
                                            ) end
                                    );

                                directory ->
                                    gleam@result:'try'(
                                        simplifile_erl:create_directory(
                                            Dest_path
                                        ),
                                        fun(_) ->
                                            do_copy_directory(
                                                Src_path,
                                                Dest_path
                                            )
                                        end
                                    );

                                symlink ->
                                    {error,
                                        {unknown,
                                            <<"This is an internal bug where the `file_info` is somehow returning info about a simlink. Please file an issue on the simplifile repo."/utf8>>}};

                                other ->
                                    {error,
                                        {unknown,
                                            <<"Unknown file type (not file, directory, or simlink)"/utf8>>}}
                            end end
                    )
                end
            ),
            {ok, nil}
        end
    ).

-file("src/simplifile.gleam", 561).
?DOC(" Copy a directory recursively\n").
-spec copy_directory(binary(), binary()) -> {ok, nil} | {error, file_error()}.
copy_directory(Src, Dest) ->
    gleam@result:'try'(
        create_directory_all(Dest),
        fun(_) -> do_copy_directory(Src, Dest) end
    ).

-file("src/simplifile.gleam", 523).
?DOC(
    " Copy a file or a directory to a new path. Copies directories recursively.\n"
    " \n"
    " ### Performance Note \n"
    " This function does work to determine if the src path\n"
    " points to a file or a directory. Consider using one of the the dedicated \n"
    " functions `copy_file` or `copy_directory` if you already know which one you need.\n"
).
-spec copy(binary(), binary()) -> {ok, nil} | {error, file_error()}.
copy(Src, Dest) ->
    gleam@result:'try'(
        simplifile_erl:file_info(Src),
        fun(Src_info) -> case file_info_type(Src_info) of
                file ->
                    copy_file(Src, Dest);

                directory ->
                    copy_directory(Src, Dest);

                symlink ->
                    {error,
                        {unknown,
                            <<"This is an internal bug where the `file_info` is somehow returning info about a simlink. Please file an issue on the simplifile repo."/utf8>>}};

                other ->
                    {error,
                        {unknown,
                            <<"Unknown file type (not file, directory, or simlink)"/utf8>>}}
            end end
    ).

-file("src/simplifile.gleam", 605).
?DOC(" Copy a directory recursively and then delete the old one.\n").
-spec rename_directory(binary(), binary()) -> {ok, nil} | {error, file_error()}.
rename_directory(Src, Dest) ->
    gleam@result:'try'(
        copy_directory(Src, Dest),
        fun(_) -> simplifile_erl:delete(Src) end
    ).

-file("src/simplifile.gleam", 615).
?DOC(
    " Clear the contents of a directory, deleting all files and directories within\n"
    " but leaving the top level directory in place.\n"
).
-spec clear_directory(binary()) -> {ok, nil} | {error, file_error()}.
clear_directory(Path) ->
    gleam@result:'try'(
        simplifile_erl:read_directory(Path),
        fun(Paths) -> _pipe = Paths,
            _pipe@1 = gleam@list:map(
                _pipe,
                fun(_capture) -> filepath:join(Path, _capture) end
            ),
            delete_all(_pipe@1) end
    ).

-file("src/simplifile.gleam", 625).
?DOC(
    " Returns a list of filepaths for every file in the directory, including nested\n"
    " files.\n"
).
-spec get_files(binary()) -> {ok, list(binary())} | {error, file_error()}.
get_files(Directory) ->
    gleam@result:'try'(
        simplifile_erl:read_directory(Directory),
        fun(Contents) ->
            gleam@list:try_fold(
                Contents,
                [],
                fun(Acc, Content) ->
                    Path = filepath:join(Directory, Content),
                    gleam@result:'try'(
                        simplifile_erl:file_info(Path),
                        fun(Info) -> case file_info_type(Info) of
                                file ->
                                    {ok, [Path | Acc]};

                                directory ->
                                    gleam@result:'try'(
                                        get_files(Path),
                                        fun(Nested_files) ->
                                            {ok,
                                                lists:append(Acc, Nested_files)}
                                        end
                                    );

                                _ ->
                                    {ok, Acc}
                            end end
                    )
                end
            )
        end
    ).

-file("src/simplifile.gleam", 648).
-spec permission_to_integer(permission()) -> integer().
permission_to_integer(Permission) ->
    case Permission of
        read ->
            8#4;

        write ->
            8#2;

        execute ->
            8#1
    end.

-file("src/simplifile.gleam", 656).
-spec integer_to_permissions(integer()) -> gleam@set:set(permission()).
integer_to_permissions(Integer) ->
    case erlang:'band'(Integer, 7) of
        7 ->
            gleam@set:from_list([read, write, execute]);

        6 ->
            gleam@set:from_list([read, write]);

        5 ->
            gleam@set:from_list([read, execute]);

        3 ->
            gleam@set:from_list([write, execute]);

        4 ->
            gleam@set:from_list([read]);

        2 ->
            gleam@set:from_list([write]);

        1 ->
            gleam@set:from_list([execute]);

        0 ->
            gleam@set:new();

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"`panic` expression evaluated."/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"simplifile"/utf8>>,
                    function => <<"integer_to_permissions"/utf8>>,
                    line => 667})
    end.

-file("src/simplifile.gleam", 680).
-spec file_permissions_to_octal(file_permissions()) -> integer().
file_permissions_to_octal(Permissions) ->
    Make_permission_digit = fun(Permissions@1) -> _pipe = Permissions@1,
        _pipe@1 = gleam@set:to_list(_pipe),
        _pipe@2 = gleam@list:map(_pipe@1, fun permission_to_integer/1),
        gleam@int:sum(_pipe@2) end,
    ((Make_permission_digit(erlang:element(2, Permissions)) * 64) + (Make_permission_digit(
        erlang:element(3, Permissions)
    )
    * 8))
    + Make_permission_digit(erlang:element(4, Permissions)).

-file("src/simplifile.gleam", 695).
-spec octal_to_file_permissions(integer()) -> file_permissions().
octal_to_file_permissions(Octal) ->
    {file_permissions,
        begin
            _pipe = Octal,
            _pipe@1 = erlang:'bsr'(_pipe, 6),
            integer_to_permissions(_pipe@1)
        end,
        begin
            _pipe@2 = Octal,
            _pipe@3 = erlang:'bsr'(_pipe@2, 3),
            integer_to_permissions(_pipe@3)
        end,
        begin
            _pipe@4 = Octal,
            integer_to_permissions(_pipe@4)
        end}.

-file("src/simplifile.gleam", 228).
?DOC(" Extract the `FilePermissions` from a given FileInfo value.\n").
-spec file_info_permissions(file_info()) -> file_permissions().
file_info_permissions(File_info) ->
    octal_to_file_permissions(file_info_permissions_octal(File_info)).

-file("src/simplifile.gleam", 731).
?DOC(
    " Sets the permissions for a given file using an octal representation\n"
    "\n"
    " # Example\n"
    " ```gleam\n"
    " set_permissions_octal(\"./script.sh\", 0o777)\n"
    " ```\n"
).
-spec set_permissions_octal(binary(), integer()) -> {ok, nil} |
    {error, file_error()}.
set_permissions_octal(Filepath, Permissions) ->
    simplifile_erl:set_permissions_octal(Filepath, Permissions).

-file("src/simplifile.gleam", 716).
?DOC(
    " Sets the permissions for a given file\n"
    "\n"
    " # Example\n"
    " ```gleam\n"
    " let all = set.from_list([Read, Write, Execute])\n"
    " let all = FilePermissions(user: all, group: all, other: all)\n"
    " let assert Ok(Nil) = set_permissions(\"./script.sh\", all)\n"
    " ```\n"
).
-spec set_permissions(binary(), file_permissions()) -> {ok, nil} |
    {error, file_error()}.
set_permissions(Filepath, Permissions) ->
    simplifile_erl:set_permissions_octal(
        Filepath,
        file_permissions_to_octal(Permissions)
    ).

-file("src/simplifile.gleam", 739).
?DOC(" Returns the current working directory\n").
-spec current_directory() -> {ok, binary()} | {error, file_error()}.
current_directory() ->
    _pipe = file:get_cwd(),
    gleam@result:map(_pipe, fun gleam_stdlib:utf_codepoint_list_to_string/1).
