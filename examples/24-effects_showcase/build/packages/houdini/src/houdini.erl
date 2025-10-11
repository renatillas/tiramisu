-module(houdini).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/houdini.gleam").
-export([escape/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/houdini.gleam", 22).
?DOC(
    " Escapes a string to be safely used inside an HTML document by escaping\n"
    " the following characters:\n"
    "   - `<` becomes `&lt;`\n"
    "   - `>` becomes `&gt;`\n"
    "   - `&` becomes `&amp;`\n"
    "   - `\"` becomes `&quot;`\n"
    "   - `'` becomes `&#39;`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " assert escape(\"wibble & wobble\") == \"wibble &amp; wobble\"\n"
    " assert escape(\"wibble > wobble\") == \"wibble &gt; wobble\"\n"
    " ```\n"
).
-spec escape(binary()) -> binary().
escape(String) ->
    houdini@internal@escape_erl:escape(String).
