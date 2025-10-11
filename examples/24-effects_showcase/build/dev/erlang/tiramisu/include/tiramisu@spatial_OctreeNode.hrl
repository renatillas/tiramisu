-record(octree_node, {
    bounds :: tiramisu@spatial:a_a_b_b(),
    capacity :: integer(),
    items :: list({vec@vec3:vec3(float()), any()}),
    children :: gleam@option:option(tiramisu@spatial:octree_children(any()))
}).
