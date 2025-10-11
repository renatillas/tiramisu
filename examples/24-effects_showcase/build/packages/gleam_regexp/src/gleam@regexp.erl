-module(gleam@regexp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compile/2, from_string/1, check/2, split/2, scan/2, replace/3, match_map/3]).
-export_type([regexp/0, match/0, compile_error/0, options/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This package uses the regular expression engine of the underlying platform.\n"
    " Regular expressions in Erlang and JavaScript largely share the same syntax, but\n"
    " there are some differences and have different performance characteristics. Be\n"
    " sure to thoroughly test your code on all platforms that you support when using\n"
    " this library.\n"
).

-type regexp() :: any().

-type match() :: {match, binary(), list(gleam@option:option(binary()))}.

-type compile_error() :: {compile_error, binary(), integer()}.

-type options() :: {options, boolean(), boolean()}.

-file("src/gleam/regexp.gleam", 56).
?DOC(
    " Creates a `Regexp` with some additional options.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let options = Options(case_insensitive: False, multi_line: True)\n"
    " let assert Ok(re) = compile(\"^[0-9]\", with: options)\n"
    " check(re, \"abc\\n123\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let options = Options(case_insensitive: True, multi_line: False)\n"
    " let assert Ok(re) = compile(\"[A-Z]\", with: options)\n"
    " check(re, \"abc123\")\n"
    " // -> True\n"
    " ```\n"
).
-spec compile(binary(), options()) -> {ok, regexp()} | {error, compile_error()}.
compile(Pattern, Options) ->
    gleam_regexp_ffi:compile(Pattern, Options).

-file("src/gleam/regexp.gleam", 93).
?DOC(
    " Creates a new `Regexp`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\"[0-9]\")\n"
    " check(re, \"abc123\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " check(re, \"abcxyz\")\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " from_string(\"[0-9\")\n"
    " // -> Error(CompileError(\n"
    " //   error: \"missing terminating ] for character class\",\n"
    " //   byte_index: 4\n"
    " // ))\n"
    " ```\n"
).
-spec from_string(binary()) -> {ok, regexp()} | {error, compile_error()}.
from_string(Pattern) ->
    compile(Pattern, {options, false, false}).

-file("src/gleam/regexp.gleam", 112).
?DOC(
    " Returns a boolean indicating whether there was a match or not.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\"^f.o.?\")\n"
    " check(with: re, content: \"foo\")\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " check(with: re, content: \"boo\")\n"
    " // -> False\n"
    " ```\n"
).
-spec check(regexp(), binary()) -> boolean().
check(Regexp, String) ->
    gleam_regexp_ffi:check(Regexp, String).

-file("src/gleam/regexp.gleam", 130).
?DOC(
    " Splits a string.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\" *, *\")\n"
    " split(with: re, content: \"foo,32, 4, 9  ,0\")\n"
    " // -> [\"foo\", \"32\", \"4\", \"9\", \"0\"]\n"
    " ```\n"
).
-spec split(regexp(), binary()) -> list(binary()).
split(Regexp, String) ->
    gleam_regexp_ffi:split(Regexp, String).

-file("src/gleam/regexp.gleam", 190).
?DOC(
    " Collects all matches of the regular expression.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = from_string(\"[oi]n a (\\\\w+)\")\n"
    " scan(with: re, content: \"I am on a boat in a lake.\")\n"
    " // -> [\n"
    " //   Match(content: \"on a boat\", submatches: [Some(\"boat\")]),\n"
    " //   Match(content: \"in a lake\", submatches: [Some(\"lake\")]),\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = regexp.from_string(\"([+|\\\\-])?(\\\\d+)(\\\\w+)?\")\n"
    " scan(with: re, content: \"-36\")\n"
    " // -> [\n"
    " //   Match(content: \"-36\", submatches: [Some(\"-\"), Some(\"36\")])\n"
    " // ]\n"
    "\n"
    " scan(with: re, content: \"36\")\n"
    " // -> [\n"
    " //   Match(content: \"36\", submatches: [None, Some(\"36\")])\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) =\n"
    "   regexp.from_string(\"var\\\\s*(\\\\w+)\\\\s*(int|string)?\\\\s*=\\\\s*(.*)\")\n"
    " scan(with: re, content: \"var age = 32\")\n"
    " // -> [\n"
    " //   Match(\n"
    " //     content: \"var age = 32\",\n"
    " //     submatches: [Some(\"age\"), None, Some(\"32\")],\n"
    " //   ),\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = regexp.from_string(\"let (\\\\w+) = (\\\\w+)\")\n"
    " scan(with: re, content: \"let age = 32\")\n"
    " // -> [\n"
    " //   Match(\n"
    " //     content: \"let age = 32\",\n"
    " //     submatches: [Some(\"age\"), Some(\"32\")],\n"
    " //   ),\n"
    " // ]\n"
    "\n"
    " scan(with: re, content: \"const age = 32\")\n"
    " // -> []\n"
    " ```\n"
).
-spec scan(regexp(), binary()) -> list(match()).
scan(Regexp, String) ->
    gleam_regexp_ffi:scan(Regexp, String).

-file("src/gleam/regexp.gleam", 216).
?DOC(
    " Creates a new `String` by replacing all substrings that match the regular\n"
    " expression.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = regexp.from_string(\"^https://\")\n"
    " replace(each: re, in: \"https://example.com\", with: \"www.\")\n"
    " // -> \"www.example.com\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = regexp.from_string(\"[, +-]\")\n"
    " replace(each: re, in: \"a,b-c d+e\", with: \"/\")\n"
    " // -> \"a/b/c/d/e\"\n"
    " ```\n"
).
-spec replace(regexp(), binary(), binary()) -> binary().
replace(Pattern, String, Substitute) ->
    gleam_regexp_ffi:replace(Pattern, String, Substitute).

-file("src/gleam/regexp.gleam", 234).
?DOC(
    " Creates a new `String` by replacing all substrings that match the regular\n"
    " expression with the result of applying the function to each match.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " let assert Ok(re) = regexp.from_string(\"\\\\w+\")\n"
    " regexp.match_map(re, \"hello, joe!\", fn(m) { string.capitalise(m.content) })\n"
    " // -> \"Hello, Joe!\"\n"
    " ```\n"
).
-spec match_map(regexp(), binary(), fun((match()) -> binary())) -> binary().
match_map(Pattern, String, Substitute) ->
    gleam_regexp_ffi:match_map(Pattern, String, Substitute).
