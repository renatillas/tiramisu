-record(mount, {
    kind :: integer(),
    open_shadow_root :: boolean(),
    will_adopt_styles :: boolean(),
    observed_attributes :: list(binary()),
    observed_properties :: list(binary()),
    requested_contexts :: list(binary()),
    provided_contexts :: gleam@dict:dict(binary(), gleam@json:json()),
    vdom :: lustre@vdom@vnode:element(any())
}).
