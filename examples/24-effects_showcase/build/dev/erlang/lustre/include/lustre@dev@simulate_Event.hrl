-record(event, {
    target :: lustre@dev@query:'query'(),
    name :: binary(),
    data :: gleam@json:json()
}).
