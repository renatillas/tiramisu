-module(lustre_dev_tools@bin@gleam).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/bin/gleam.gleam").
-export([build/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_dev_tools/bin/gleam.gleam", 6).
?DOC(false).
-spec build(lustre_dev_tools@project:project()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
build(_) ->
    gleam@result:'try'(
        begin
            _pipe = system_ffi:run(<<"gleam build --target javascript"/utf8>>),
            gleam@result:map_error(
                _pipe,
                fun(_capture) ->
                    {external_command_failed, <<"gleam"/utf8>>, _capture}
                end
            )
        end,
        fun(_) -> {ok, nil} end
    ).
