-record(project, {
    name :: binary(),
    options :: gleam@dict:dict(binary(), tom:toml()),
    root :: binary(),
    src :: binary(),
    dev :: binary(),
    assets :: binary(),
    bin :: binary(),
    build :: binary(),
    has_node_modules :: boolean()
}).
