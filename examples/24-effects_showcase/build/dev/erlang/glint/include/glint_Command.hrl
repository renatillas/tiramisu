-record(command, {
    do :: fun((glint:named_args(), list(binary()), glint:flags()) -> any()),
    flags :: glint:flags(),
    description :: binary(),
    unnamed_args :: gleam@option:option(glint:args_count()),
    named_args :: list(binary())
}).
