-record(file_permissions, {
    user :: gleam@set:set(simplifile:permission()),
    group :: gleam@set:set(simplifile:permission()),
    other :: gleam@set:set(simplifile:permission())
}).
