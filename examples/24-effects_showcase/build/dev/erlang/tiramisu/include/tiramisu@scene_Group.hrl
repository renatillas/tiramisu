-record(group, {
    id :: any(),
    transform :: tiramisu@transform:transform(),
    children :: list(tiramisu@scene:node_(any()))
}).
