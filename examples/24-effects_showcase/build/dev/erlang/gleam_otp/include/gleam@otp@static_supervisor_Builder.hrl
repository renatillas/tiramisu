-record(builder, {
    strategy :: gleam@otp@static_supervisor:strategy(),
    intensity :: integer(),
    period :: integer(),
    auto_shutdown :: gleam@otp@static_supervisor:auto_shutdown(),
    children :: list(gleam@otp@supervision:child_specification(nil))
}).
