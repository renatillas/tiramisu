-module(lustre_dev_tools@error).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre_dev_tools/error.gleam").
-export([explain/1]).
-export_type([error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type error() :: {could_not_download_bun_binary, gleam@httpc:http_error()} |
    {could_not_download_tailwind_binary, gleam@httpc:http_error()} |
    {could_not_extract_bun_archive, binary(), binary(), binary()} |
    {could_not_initialise_dev_tools, simplifile:file_error()} |
    {could_not_locate_bun_binary, binary()} |
    {could_not_locate_tailwind_binary, binary()} |
    {could_not_read_file, binary(), simplifile:file_error()} |
    {could_not_set_file_permissions, binary(), simplifile:file_error()} |
    {could_not_start_dev_server, gleam@otp@actor:start_error()} |
    {could_not_start_file_watcher, binary(), binary(), binary()} |
    {could_not_verify_bun_hash, binary(), binary()} |
    {could_not_verify_tailwind_hash, binary(), binary()} |
    {could_not_write_file, binary(), simplifile:file_error()} |
    {external_command_failed, binary(), binary()} |
    {failed_to_build_project, binary()} |
    {missing_required_flag, list(binary())} |
    {must_be_project_root, binary()} |
    proxy_invalid_to |
    proxy_missing_from |
    proxy_missing_to |
    {unknown_build_tool, binary()} |
    {unknown_gleam_module, binary()} |
    {unknown_integration, binary()} |
    {unsupported_bun_version, binary(), binary(), binary()} |
    {unsupported_platform, binary(), binary()} |
    {unsupported_tailwind_version, binary(), binary(), binary()}.

-file("src/lustre_dev_tools/error.gleam", 36).
?DOC(false).
-spec explain(error()) -> binary().
explain(Error) ->
    case Error of
        {could_not_download_bun_binary, Reason} ->
            _pipe = <<"
I ran into a problem while trying to download the Bun binary. Here's the error I
got from the HTTP client:

  ${reason}

Make sure you're connected to the internet and that no firewall or proxy is
blocking connections to GitHub.

Hint: you can provide a path to a local Bun binary by setting the
`tools.lustre.bin.bun` field in your `gleam.toml`. Use the string `\"system\"`
to use the Bun binary accessible in your system path.
      "/utf8>>,
            gleam@string:replace(
                _pipe,
                <<"${reason}"/utf8>>,
                gleam@string:inspect(Reason)
            );

        {could_not_download_tailwind_binary, Reason@1} ->
            _pipe@1 = <<"
I ran into a problem while trying to download the Tailwind binary. Here's the
error I got from the HTTP client:

  ${reason}

Make sure you're connected to the internet and that no firewall or proxy is
blocking connections to GitHub.

Hint: you can provide a path to a local Tailwind binary by setting the
`tools.lustre.bin.tailwind` field in your `gleam.toml`. Use the string `\"system\"`
to use the Tailwind binary accessible in your system path.
        "/utf8>>,
            gleam@string:replace(
                _pipe@1,
                <<"${reason}"/utf8>>,
                gleam@string:inspect(Reason@1)
            );

        {could_not_extract_bun_archive, Os, Arch, Version} ->
            _pipe@2 = <<"
I ran into an unexpected problem while trying to extract the Bun archive. This
might happen if the archive is corrupted or has changed format. Please open an
issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - os: ${os}
  - arch: ${arch}
  - version: ${version}

Hint: you can provide a path to a local Tailwind binary by setting the
`tools.lustre.bin.bun` field in your `gleam.toml`. Use the string `\"system\"`
to use the Tailwind binary accessible in your system path.
      "/utf8>>,
            _pipe@3 = gleam@string:replace(_pipe@2, <<"${os}"/utf8>>, Os),
            _pipe@4 = gleam@string:replace(_pipe@3, <<"${arch}"/utf8>>, Arch),
            gleam@string:replace(_pipe@4, <<"${version}"/utf8>>, Version);

        {could_not_locate_bun_binary, Path} ->
            _pipe@5 = <<"
I ran into a problem while trying to run the Bun binary at the following path:

  ${path}

If you are trying to use a local binary, make sure the path is correct and that
relative paths are relative to the project's root directory.
      "/utf8>>,
            gleam@string:replace(_pipe@5, <<"${path}"/utf8>>, Path);

        {could_not_locate_tailwind_binary, Path@1} ->
            _pipe@6 = <<"
I ran into a problem while trying to run the Tailwind binary at the following
path:

  ${path}

If you are trying to use a local binary, make sure the path is correct and that
relative paths are relative to the current working directory.
      "/utf8>>,
            gleam@string:replace(_pipe@6, <<"${path}"/utf8>>, Path@1);

        {could_not_initialise_dev_tools, Reason@2} ->
            _pipe@7 = <<"
I ran into a problem while setting up. I need to create some directories and
modify your `.gitignore` if you have one. Here's the error I got from the
file system:

  ${reason}

Make sure you have permissions to write to the current directory.
      "/utf8>>,
            gleam@string:replace(
                _pipe@7,
                <<"${reason}"/utf8>>,
                gleam@string:inspect(Reason@2)
            );

        {could_not_read_file, Path@2, Reason@3} ->
            _pipe@8 = <<"
I ran into a problem while trying to read a file at the following path:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure the file exists and that you have permissions to read it.
      "/utf8>>,
            _pipe@9 = gleam@string:replace(_pipe@8, <<"${path}"/utf8>>, Path@2),
            gleam@string:replace(
                _pipe@9,
                <<"${reason}"/utf8>>,
                gleam@string:inspect(Reason@3)
            );

        {could_not_set_file_permissions, Path@3, Reason@4} ->
            _pipe@10 = <<"
I ran into a problem while trying to set the file permissions for an integration
at:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure you have the necessary permissions to write to this file.

Hint: you can provide a path to local binaries for Lustre to use instead by
adding the `tools.lustre.bin` table to your `gleam.toml`. Consult the TOML
reference on HexDocs for more information.
      "/utf8>>,
            _pipe@11 = gleam@string:replace(
                _pipe@10,
                <<"${path}"/utf8>>,
                Path@3
            ),
            gleam@string:replace(
                _pipe@11,
                <<"${reason}"/utf8>>,
                gleam@string:inspect(Reason@4)
            );

        {could_not_start_file_watcher, Watcher, Os@1, Arch@1} ->
            _pipe@12 = <<"
I ran into a problem while trying to start the file watcher. This might be due
to an incompatibility with your platform or a bug in the watcher code. Please
open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - watcher: ${watcher}
  - os: ${os}
  - arch: ${arch}
      "/utf8>>,
            _pipe@13 = gleam@string:replace(
                _pipe@12,
                <<"${watcher}"/utf8>>,
                Watcher
            ),
            _pipe@14 = gleam@string:replace(_pipe@13, <<"${os}"/utf8>>, Os@1),
            gleam@string:replace(_pipe@14, <<"${arch}"/utf8>>, Arch@1);

        {could_not_verify_bun_hash, Expected, Actual} ->
            _pipe@15 = <<"
I ran into a problem while trying to verify the integrity of the Bun archive I
just downloaded. The expected hash was:

  ${expected}

But the actual hash was:

  ${actual}

This can happen if you have a proxy or firewall that interferes with the download.
If you think this is a bug, please open an issue at:

https://github.com/lustre-labs/dev-tools/issues/new

Hint: you can provide a path to a local Bun binary by setting the
`tools.lustre.bin.bun` field in your `gleam.toml`. Use the string `\"system\"`
to use the Bun binary accessible in your system path.
      "/utf8>>,
            _pipe@16 = gleam@string:replace(
                _pipe@15,
                <<"${expected}"/utf8>>,
                Expected
            ),
            gleam@string:replace(_pipe@16, <<"${actual}"/utf8>>, Actual);

        {could_not_verify_tailwind_hash, Expected@1, Actual@1} ->
            _pipe@17 = <<"
I ran into a problem while trying to verify the integrity of the Tailwind binary
I just downloaded. The expected hash was:

  ${expected}

But the actual hash was:

  ${actual}

This can happen if you have a proxy or firewall that interferes with the download.
If you think this is a bug, please open an issue at:

https://github.com/lustre-labs/dev-tools/issues/new

Hint: you can provide a path to a local Tailwind binary by setting the
`tools.lustre.bin.tailwind` field in your `gleam.toml`. Use the string `\"system\"`
to use the Tailwind binary accessible in your system path.
        "/utf8>>,
            _pipe@18 = gleam@string:replace(
                _pipe@17,
                <<"${expected}"/utf8>>,
                Expected@1
            ),
            gleam@string:replace(_pipe@18, <<"${actual}"/utf8>>, Actual@1);

        {could_not_write_file, Path@4, Reason@5} ->
            _pipe@19 = <<"
I ran into a problem while trying to write a file at the following path:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure you have permissions to write files in this directory.
      "/utf8>>,
            _pipe@20 = gleam@string:replace(
                _pipe@19,
                <<"${path}"/utf8>>,
                Path@4
            ),
            gleam@string:replace(
                _pipe@20,
                <<"${reason}"/utf8>>,
                gleam@string:inspect(Reason@5)
            );

        {external_command_failed, Command, Reason@6} ->
            _pipe@21 = <<"
I ran into a problem while trying to run the following command:

  ${command}

Here's the error I got from the command:

  ${reason}
      "/utf8>>,
            _pipe@22 = gleam@string:replace(
                _pipe@21,
                <<"${command}"/utf8>>,
                Command
            ),
            _pipe@23 = gleam@string:replace(
                _pipe@22,
                <<"${reason}"/utf8>>,
                Reason@6
            ),
            _pipe@24 = gleam@string:replace(
                _pipe@23,
                <<"${os}"/utf8>>,
                system_ffi:detect_os()
            ),
            gleam@string:replace(
                _pipe@24,
                <<"${arch}"/utf8>>,
                system_ffi:detect_arch()
            );

        {failed_to_build_project, Reason@7} ->
            _pipe@25 = <<"
I ran into a problem while trying to build your application. Here's the error I
got while building:

  ${reason}

Make sure your Gleam code compiles without errors and any entry points point to
Gleam modules.
      "/utf8>>,
            gleam@string:replace(_pipe@25, <<"${reason}"/utf8>>, Reason@7);

        {missing_required_flag, Name} ->
            _pipe@26 = <<"
I'm missing at least one required flag to run this command. Please make sure you
provide the `--${name}` flag when running the command or configure your `gleam.toml`
to include the `tools.lustre.${path}` field.
      "/utf8>>,
            _pipe@27 = gleam@string:replace(
                _pipe@26,
                <<"${name}"/utf8>>,
                gleam@string:join(Name, <<"-"/utf8>>)
            ),
            gleam@string:replace(
                _pipe@27,
                <<"${path}"/utf8>>,
                gleam@string:join(Name, <<"."/utf8>>)
            );

        {must_be_project_root, Path@5} ->
            _pipe@28 = <<"
I need to be run from the root directory of a Gleam project. I looked for a
`gleam.toml` and found one at:

  ${path}

Please run me from the directory that contains that file!
      "/utf8>>,
            gleam@string:replace(_pipe@28, <<"${path}"/utf8>>, Path@5);

        proxy_invalid_to ->
            <<"
I ran into a problem trying to set up the proxy you provided. The `to` URL
looks invalid. Please make sure you provide a valid URL for the `to` field.
      "/utf8>>;

        proxy_missing_from ->
            <<"
I ran into a problem trying to set up the proxy you provided. The `from` field
is missing. Please make sure you provide a value for the `from` field like
`\"/api\"`.
      "/utf8>>;

        proxy_missing_to ->
            <<"
I ran into a problem trying to set up the proxy you provided. The `to` field
is missing. Please make sure you provide a value for the `to` field like
`\"http://localhost:3000\"`. This should be the full URL of the server you want
to proxy requests to.
      "/utf8>>;

        {unknown_build_tool, Name@1} ->
            _pipe@29 = <<"
I ran into a problem trying to eject your project from Lustre Dev Tools. I don't
know how to eject for the build tool `${name}`. Currently I can generate the
config required for either Bun or Vite.

If you need to use a different build tool, please configure the project yourself.
If you think this is a bug, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "/utf8>>,
            gleam@string:replace(_pipe@29, <<"${name}"/utf8>>, Name@1);

        {unknown_gleam_module, Name@2} ->
            _pipe@30 = <<"
I ran into a problem while trying to build your application. I couldn't find the
entry module you provided:

  ${name}

Make sure the module exists in your project and the name is correct. Gleam module
names are the full path from the `src` directory like `wibble/wobble/woo`.
      "/utf8>>,
            gleam@string:replace(_pipe@30, <<"${name}"/utf8>>, Name@2);

        {unknown_integration, Name@3} ->
            _pipe@31 = <<"
I don't know how to add the integration `${name}`. Currently I have integrations
for Bun and Tailwind. If you have suggestions for other integrations we could
support in the future, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "/utf8>>,
            gleam@string:replace(_pipe@31, <<"${name}"/utf8>>, Name@3);

        {unsupported_bun_version, Path@6, Expected@2, Actual@2} ->
            _pipe@32 = <<"
I ran into a problem while trying to verify the version of the Bun binary at:

  ${path}

The expected version was:

  ${expected}

But the actual version was:

  ${actual}

If you are supplying a path to a local Bun binary, make sure it matches the
version I expect!
      "/utf8>>,
            _pipe@33 = gleam@string:replace(
                _pipe@32,
                <<"${path}"/utf8>>,
                Path@6
            ),
            _pipe@34 = gleam@string:replace(
                _pipe@33,
                <<"${expected}"/utf8>>,
                Expected@2
            ),
            _pipe@35 = gleam@string:replace(
                _pipe@34,
                <<"${actual}"/utf8>>,
                Actual@2
            ),
            gleam@string:trim(_pipe@35);

        {unsupported_tailwind_version, Path@7, Expected@3, Actual@3} ->
            _pipe@36 = <<"
I ran into a problem while trying to verify the version of the Tailwind binary
at:

  ${path}

The expected version was:

  ${expected}

But the actual version was:

  ${actual}

If you are supplying a path to a local Bun binary, make sure it matches the
version I expect!
        "/utf8>>,
            _pipe@37 = gleam@string:replace(
                _pipe@36,
                <<"${path}"/utf8>>,
                Path@7
            ),
            _pipe@38 = gleam@string:replace(
                _pipe@37,
                <<"${expected}"/utf8>>,
                Expected@3
            ),
            _pipe@39 = gleam@string:replace(
                _pipe@38,
                <<"${actual}"/utf8>>,
                Actual@3
            ),
            gleam@string:trim(_pipe@39);

        {unsupported_platform, Os@2, Arch@2} ->
            _pipe@40 = <<"
Unfortunately, I don't support the platform you're running on. Currently, I
only support 64-bit Linux, macOS, and Windows systems. It would be helpful if
you could open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - os: ${os}
  - arch: ${arch}

So we can consider how to support more users in the future. In the meantime, I
suggest you take a look at https://vite.dev or https://esbuild.github.io as
alternatives for building your Lustre applications.
      "/utf8>>,
            _pipe@41 = gleam@string:replace(_pipe@40, <<"${os}"/utf8>>, Os@2),
            gleam@string:replace(_pipe@41, <<"${arch}"/utf8>>, Arch@2);

        {could_not_start_dev_server, Reason@8} ->
            _pipe@42 = <<"
I ran into a problem while trying to start the development server. Here's what I
got:

  ${reason}

Make sure the port you're trying to use is not already in use by another program.
      "/utf8>>,
            gleam@string:replace(
                _pipe@42,
                <<"${reason}"/utf8>>,
                gleam@string:inspect(Reason@8)
            )
    end.
