-module(platform).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([runtime/0, os/0, arch/0]).
-export_type([runtime/0, os/0, arch/0]).

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
    ppc |
    ppc64 |
    risc_v64 |
    s390 |
    s390x |
    {other_arch, binary()}.

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
            ppc;

        <<"ppc64"/utf8>> ->
            ppc64;

        <<"riscv64"/utf8>> ->
            risc_v64;

        <<"s390"/utf8>> ->
            s390;

        <<"s390x"/utf8>> ->
            s390x;

        Arch ->
            {other_arch, Arch}
    end.
