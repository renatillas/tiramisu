-module(gleam@erlang@atom).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/erlang/atom.gleam").
-export([get/1, create/1, to_string/1, to_dynamic/1, cast_from_dynamic/1, decoder/0]).
-export_type([atom_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type atom_() :: any().

-file("src/gleam/erlang/atom.gleam", 27).
?DOC(
    " Finds an existing atom for the given string.\n"
    "\n"
    " If no atom is found in the virtual machine's atom table for the string then\n"
    " an error is returned.\n"
).
-spec get(binary()) -> {ok, atom_()} | {error, nil}.
get(A) ->
    gleam_erlang_ffi:atom_from_string(A).

-file("src/gleam/erlang/atom.gleam", 39).
?DOC(
    " Creates an atom from a string, inserting a new value into the virtual\n"
    " machine's atom table if an atom does not already exist for the given\n"
    " string.\n"
    "\n"
    " We must be careful when using this function as there is a limit to the\n"
    " number of atom that can fit in the virtual machine's atom table. Never\n"
    " convert user input into atoms as filling the atom table will cause the\n"
    " virtual machine to crash!\n"
).
-spec create(binary()) -> atom_().
create(A) ->
    erlang:binary_to_atom(A).

-file("src/gleam/erlang/atom.gleam", 52).
?DOC(
    " Returns a `String` corresponding to the text representation of the given\n"
    " `Atom`.\n"
    "\n"
    " ## Examples\n"
    " ```gleam\n"
    " let ok_atom = create(\"ok\")\n"
    " to_string(ok_atom)\n"
    " // -> \"ok\"\n"
    " ```\n"
).
-spec to_string(atom_()) -> binary().
to_string(A) ->
    erlang:atom_to_binary(A).

-file("src/gleam/erlang/atom.gleam", 59).
?DOC(
    " Convert an atom to a dynamic value, throwing away the type information. \n"
    "\n"
    " This may be useful for testing decoders.\n"
).
-spec to_dynamic(atom_()) -> gleam@dynamic:dynamic_().
to_dynamic(A) ->
    gleam_erlang_ffi:identity(A).

-file("src/gleam/erlang/atom.gleam", 62).
-spec cast_from_dynamic(gleam@dynamic:dynamic_()) -> atom_().
cast_from_dynamic(A) ->
    gleam_erlang_ffi:identity(A).

-file("src/gleam/erlang/atom.gleam", 71).
?DOC(
    " A dynamic decoder for atoms.\n"
    "\n"
    " You almost certainly should not use this to work with externally defined\n"
    " functions. They return known types, so you should define the external\n"
    " functions with the correct types, defining wrapper functions in Erlang if\n"
    " the external types cannot be mapped directly onto Gleam types.\n"
).
-spec decoder() -> gleam@dynamic@decode:decoder(atom_()).
decoder() ->
    gleam@dynamic@decode:new_primitive_decoder(
        <<"Atom"/utf8>>,
        fun(Data) -> case erlang:is_atom(Data) of
                true ->
                    {ok, gleam_erlang_ffi:identity(Data)};

                false ->
                    {error, erlang:binary_to_atom(<<"nil"/utf8>>)}
            end end
    ).
