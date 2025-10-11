-record(config, {
    name :: gleam@option:option(binary()),
    usage_colour :: gleam@option:option(gleam_community@colour:colour()),
    flags_colour :: gleam@option:option(gleam_community@colour:colour()),
    subcommands_colour :: gleam@option:option(gleam_community@colour:colour()),
    as_module :: boolean(),
    description :: gleam@option:option(binary()),
    indent_width :: integer(),
    max_output_width :: integer(),
    min_first_column_width :: integer(),
    column_gap :: integer(),
    flag_prefix :: binary(),
    flag_delimiter :: binary()
}).
