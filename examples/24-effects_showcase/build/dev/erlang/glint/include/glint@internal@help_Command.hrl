-record(command, {
    meta :: glint@internal@help:metadata(),
    flags :: list(glint@internal@help:flag()),
    subcommands :: list(glint@internal@help:metadata()),
    unnamed_args :: gleam@option:option(glint@internal@help:args_count()),
    named_args :: list(binary())
}).
