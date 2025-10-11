-module(platform).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/platform.gleam").
-export([runtime/0, os/0, arch/0]).
-export_type([runtime/0, os/0, arch/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type runtime() :: erlang |
    node |
    bun |
    deno |
    browser |
    {other_runtime, binary()}.

-type os() :: aix |
    darwin |
    free_bsd |
    linux |
    open_bsd |
    sun_os |
    win32 |
    {other_os, binary()}.

-type arch() :: arm |
    arm64 |
    x86 |
    x64 |
    loong64 |
    mips |
    mips_little_endian |
    p_p_c |
    p_p_c64 |
    risc_v64 |
    s390 |
    s390_x |
    {other_arch, binary()}.

-file("src/platform.gleam", 54).
?DOC(
    " Returns the runtime this application is running on\n"
    " \n"
    " On the erlang target, it'll always return `Erlang`\n"
    " \n"
    " On the js target, it'll try to detect the js runtime\n"
).
-spec runtime() -> runtime().
runtime() ->
    case platform_ffi:runtime() of
        <<"erlang"/utf8>> ->
            erlang;

        <<"node"/utf8>> ->
            node;

        <<"bun"/utf8>> ->
            bun;

        <<"deno"/utf8>> ->
            deno;

        <<"browser"/utf8>> ->
            browser;

        Runtime ->
            {other_runtime, Runtime}
    end.

-file("src/platform.gleam", 68).
?DOC(
    " Returns the host operating system this appication is running on\n"
    " \n"
    " In web browsers, this will always return OtherOs(\"unknown\")\n"
).
-spec os() -> os().
os() ->
    case platform_ffi:os() of
        <<"aix"/utf8>> ->
            aix;

        <<"darwin"/utf8>> ->
            darwin;

        <<"freebsd"/utf8>> ->
            free_bsd;

        <<"linux"/utf8>> ->
            linux;

        <<"openbsd"/utf8>> ->
            open_bsd;

        <<"sunos"/utf8>> ->
            sun_os;

        <<"win32"/utf8>> ->
            win32;

        <<"win"/utf8, _/binary>> ->
            win32;

        Os ->
            {other_os, Os}
    end.

-file("src/platform.gleam", 90).
?DOC(
    " Returns the CPU architecture of the host system\n"
    " \n"
    " In web browsers, this will always return OtherArch(\"unknown\")\n"
    " \n"
    " On erlang for windows, it'll always return either X86 or X64, even under a beam vm compiled for arm.\n"
    " This is because there is no simple way to get the cpu archictecture on windows, and it currently uses \n"
    " the bitness of the cpu to guess it instead. \n"
    " \n"
    " As of 22nd August 2024, there are no prebuilt binaries for windows for arm, so this shouldn't matter\n"
).
-spec arch() -> arch().
arch() ->
    case platform_ffi:arch() of
        <<"arm"/utf8>> ->
            arm;

        <<"arm64"/utf8>> ->
            arm64;

        <<"aarch64"/utf8>> ->
            arm64;

        <<"x86"/utf8>> ->
            x86;

        <<"ia32"/utf8>> ->
            x86;

        <<"x64"/utf8>> ->
            x64;

        <<"x86_64"/utf8>> ->
            x64;

        <<"amd64"/utf8>> ->
            x64;

        <<"loong64"/utf8>> ->
            loong64;

        <<"mips"/utf8>> ->
            mips;

        <<"mipsel"/utf8>> ->
            mips_little_endian;

        <<"ppc"/utf8>> ->
            p_p_c;

        <<"ppc64"/utf8>> ->
            p_p_c64;

        <<"riscv64"/utf8>> ->
            risc_v64;

        <<"s390"/utf8>> ->
            s390;

        <<"s390x"/utf8>> ->
            s390_x;

        Arch ->
            {other_arch, Arch}
    end.
