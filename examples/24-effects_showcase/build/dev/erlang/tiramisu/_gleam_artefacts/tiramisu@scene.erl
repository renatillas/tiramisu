-module(tiramisu@scene).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/tiramisu/scene.gleam").
-export([lod_level/2, diff/2]).
-export_type([l_o_d_level/1, node_/1, patch/1, node_with_parent/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Scene graph module - declarative 3D scene construction.\n"
    "\n"
    " This module provides types and functions for building 3D scenes declaratively.\n"
    " Scenes are composed of `SceneNode` values that describe meshes, lights, cameras, and groups.\n"
    "\n"
    " ## Core Concepts\n"
    "\n"
    " - **Immutability**: Scene nodes are immutable values. Updates create new nodes.\n"
    " - **Hierarchy**: Use `Group` nodes to create parent-child relationships.\n"
    " - **Validation**: Geometry and material constructors return `Result` to catch invalid parameters.\n"
    " - **Performance**: Use `InstancedMesh` for many identical objects (1 draw call instead of thousands).\n"
    "\n"
    " ## Quick Example\n"
    "\n"
    " ```gleam\n"
    " import tiramisu/scene\n"
    " import tiramisu/transform\n"
    " import gleam/option\n"
    " import vec/vec3\n"
    "\n"
    " pub fn view(model: Model) {\n"
    "   let assert Ok(geometry) = scene.box(width: 1.0, height: 1.0, depth: 1.0)\n"
    "   let assert Ok(material) = scene.basic_material(color: 0xff0000, transparent: False, opacity: 1.0)\n"
    "\n"
    "   [\n"
    "     scene.Mesh(\n"
    "       id: \"player\",\n"
    "       geometry: geometry,\n"
    "       material: material,\n"
    "       transform: transform.at(vec3.Vec3(0.0, 1.0, 0.0)),\n"
    "       physics: option.None,\n"
    "     ),\n"
    "     scene.Light(\n"
    "       id: \"sun\",\n"
    "       light: scene.DirectionalLight(color: 0xffffff, intensity: 1.0),\n"
    "       transform: transform.identity,\n"
    "     ),\n"
    "   ]\n"
    " }\n"
    " ```\n"
).

-type l_o_d_level(ASSP) :: {l_o_d_level, float(), node_(ASSP)}.

-type node_(ASSQ) :: {mesh,
        ASSQ,
        tiramisu@geometry:geometry(),
        tiramisu@material:material(),
        tiramisu@transform:transform(),
        gleam@option:option(tiramisu@physics:rigid_body())} |
    {instanced_mesh,
        ASSQ,
        tiramisu@geometry:geometry(),
        tiramisu@material:material(),
        list(tiramisu@transform:transform())} |
    {group, ASSQ, tiramisu@transform:transform(), list(node_(ASSQ))} |
    {light, ASSQ, tiramisu@light:light(), tiramisu@transform:transform()} |
    {camera,
        ASSQ,
        tiramisu@camera:camera(),
        tiramisu@transform:transform(),
        gleam@option:option(vec@vec3:vec3(float())),
        boolean(),
        gleam@option:option({integer(), integer(), integer(), integer()})} |
    {l_o_d, ASSQ, list(l_o_d_level(ASSQ)), tiramisu@transform:transform()} |
    {model3_d,
        ASSQ,
        tiramisu@object3d:object3_d(),
        tiramisu@transform:transform(),
        gleam@option:option(tiramisu@object3d:animation_playback()),
        gleam@option:option(tiramisu@physics:rigid_body())} |
    {audio, ASSQ, tiramisu@audio:audio()} |
    {particles,
        ASSQ,
        tiramisu@particle_emitter:particle_emitter(),
        tiramisu@transform:transform(),
        boolean()} |
    {debug_box, ASSQ, vec@vec3:vec3(float()), vec@vec3:vec3(float()), integer()} |
    {debug_sphere, ASSQ, vec@vec3:vec3(float()), float(), integer()} |
    {debug_line,
        ASSQ,
        vec@vec3:vec3(float()),
        vec@vec3:vec3(float()),
        integer()} |
    {debug_axes, ASSQ, vec@vec3:vec3(float()), float()} |
    {debug_grid, ASSQ, float(), integer(), integer()} |
    {debug_point, ASSQ, vec@vec3:vec3(float()), float(), integer()}.

-type patch(ASSR) :: {add_node, ASSR, node_(ASSR), gleam@option:option(ASSR)} |
    {remove_node, ASSR} |
    {update_transform, ASSR, tiramisu@transform:transform()} |
    {update_material, ASSR, tiramisu@material:material()} |
    {update_geometry, ASSR, tiramisu@geometry:geometry()} |
    {update_light, ASSR, tiramisu@light:light()} |
    {update_animation,
        ASSR,
        gleam@option:option(tiramisu@object3d:animation_playback())} |
    {update_physics, ASSR, gleam@option:option(tiramisu@physics:rigid_body())} |
    {update_audio, ASSR, tiramisu@audio:audio()} |
    {update_instances, ASSR, list(tiramisu@transform:transform())} |
    {update_l_o_d_levels, ASSR, list(l_o_d_level(ASSR))} |
    {update_camera,
        ASSR,
        tiramisu@camera:camera(),
        gleam@option:option(vec@vec3:vec3(float()))} |
    {set_active_camera, ASSR} |
    {update_particle_emitter,
        ASSR,
        tiramisu@particle_emitter:particle_emitter()} |
    {update_particle_active, ASSR, boolean()}.

-type node_with_parent(ASSS) :: {node_with_parent,
        node_(ASSS),
        gleam@option:option(ASSS)}.

-file("src/tiramisu/scene.gleam", 90).
?DOC(
    " Create an LOD level with a distance threshold and scene node.\n"
    "\n"
    " Levels should be ordered from closest (distance: 0.0) to farthest.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " let high_detail = scene.lod_level(distance: 0.0, node: detailed_mesh)\n"
    " let low_detail = scene.lod_level(distance: 100.0, node: simple_mesh)\n"
    " ```\n"
).
-spec lod_level(float(), node_(ASST)) -> l_o_d_level(ASST).
lod_level(Distance, Node) ->
    {l_o_d_level, Distance, Node}.

-file("src/tiramisu/scene.gleam", 227).
-spec flatten_scene_helper(
    list(node_(ASTC)),
    gleam@option:option(ASTC),
    gleam@dict:dict(ASTC, node_with_parent(ASTC))
) -> gleam@dict:dict(ASTC, node_with_parent(ASTC)).
flatten_scene_helper(Nodes, Parent_id, Acc) ->
    gleam@list:fold(
        Nodes,
        Acc,
        fun(Acc@1, Node) ->
            Acc@2 = gleam@dict:insert(
                Acc@1,
                erlang:element(2, Node),
                {node_with_parent, Node, Parent_id}
            ),
            case Node of
                {group, _, _, Children} ->
                    flatten_scene_helper(
                        Children,
                        {some, erlang:element(2, Node)},
                        Acc@2
                    );

                _ ->
                    Acc@2
            end
        end
    ).

-file("src/tiramisu/scene.gleam", 223).
-spec flatten_scene(list(node_(ASSW))) -> gleam@dict:dict(ASSW, node_with_parent(ASSW)).
flatten_scene(Nodes) ->
    flatten_scene_helper(Nodes, none, maps:new()).

-file("src/tiramisu/scene.gleam", 375).
?DOC(
    " Efficiently concatenate multiple lists using fold + prepend\n"
    " O(n) total instead of list.flatten's O(n * m)\n"
).
-spec concat_patches(list(list(patch(ASUE)))) -> list(patch(ASUE)).
concat_patches(Lists) ->
    _pipe = gleam@list:fold(
        Lists,
        [],
        fun(Acc, Patches) ->
            gleam@list:fold(
                Patches,
                Acc,
                fun(Acc2, Patch) -> [Patch | Acc2] end
            )
        end
    ),
    lists:reverse(_pipe).

-file("src/tiramisu/scene.gleam", 327).
?DOC(
    " Batch patches by type for optimal rendering order\n"
    " Optimized: Single-pass partitioning + manual concatenation (no list.flatten)\n"
).
-spec batch_patches(
    list(patch(ASTT)),
    list(patch(ASTT)),
    list(patch(ASTT)),
    list(patch(ASTT))
) -> list(patch(ASTT)).
batch_patches(Removals, Parent_change_removals, Updates, Additions) ->
    {Transform_updates, Material_updates, Geometry_updates, Misc_updates} = gleam@list:fold(
        Updates,
        {[], [], [], []},
        fun(Acc, Patch) ->
            {Transforms, Materials, Geometries, Misc} = Acc,
            case Patch of
                {update_transform, _, _} ->
                    {[Patch | Transforms], Materials, Geometries, Misc};

                {update_material, _, _} ->
                    {Transforms, [Patch | Materials], Geometries, Misc};

                {update_geometry, _, _} ->
                    {Transforms, Materials, [Patch | Geometries], Misc};

                _ ->
                    {Transforms, Materials, Geometries, [Patch | Misc]}
            end
        end
    ),
    concat_patches(
        [Removals,
            Parent_change_removals,
            lists:reverse(Transform_updates),
            lists:reverse(Material_updates),
            lists:reverse(Geometry_updates),
            lists:reverse(Misc_updates),
            Additions]
    ).

-file("src/tiramisu/scene.gleam", 421).
?DOC(" Calculate the depth of a node in the hierarchy (0 = root)\n").
-spec calculate_depth(
    gleam@option:option(ASUS),
    gleam@dict:dict(ASUS, node_with_parent(ASUS)),
    integer()
) -> integer().
calculate_depth(Parent_id, Node_dict, Current_depth) ->
    case Parent_id of
        none ->
            Current_depth;

        {some, Id} ->
            case gleam_stdlib:map_get(Node_dict, Id) of
                {ok, {node_with_parent, _, Parent_parent_id}} ->
                    calculate_depth(
                        Parent_parent_id,
                        Node_dict,
                        Current_depth + 1
                    );

                {error, _} ->
                    Current_depth + 1
            end
    end.

-file("src/tiramisu/scene.gleam", 384).
?DOC(
    " Sort AddNode patches so that parents are added before their children\n"
    " Optimized: pre-compute depths as tuples to avoid dict lookups in comparator\n"
).
-spec sort_patches_by_hierarchy(
    list(patch(ASUK)),
    gleam@dict:dict(ASUK, node_with_parent(ASUK))
) -> list(patch(ASUK)).
sort_patches_by_hierarchy(Patches, Node_dict) ->
    Patches_with_depth = gleam@list:map(Patches, fun(Patch) -> case Patch of
                {add_node, _, _, Parent_id} ->
                    Depth = calculate_depth(Parent_id, Node_dict, 0),
                    {Depth, Patch};

                _ ->
                    {0, Patch}
            end end),
    _pipe = gleam@list:sort(
        Patches_with_depth,
        fun(A, B) ->
            {Depth_a, _} = A,
            {Depth_b, _} = B,
            case Depth_a < Depth_b of
                true ->
                    lt;

                false ->
                    case Depth_a > Depth_b of
                        true ->
                            gt;

                        false ->
                            eq
                    end
            end
        end
    ),
    gleam@list:map(
        _pipe,
        fun(Tuple) ->
            {_, Patch@1} = Tuple,
            Patch@1
        end
    ).

-file("src/tiramisu/scene.gleam", 546).
?DOC(" Compare Mesh fields using accumulator pattern (no empty list allocations)\n").
-spec compare_mesh_fields(
    ASVH,
    tiramisu@geometry:geometry(),
    tiramisu@material:material(),
    tiramisu@transform:transform(),
    gleam@option:option(tiramisu@physics:rigid_body()),
    tiramisu@geometry:geometry(),
    tiramisu@material:material(),
    tiramisu@transform:transform(),
    gleam@option:option(tiramisu@physics:rigid_body())
) -> list(patch(ASVH)).
compare_mesh_fields(
    Id,
    Prev_geom,
    Prev_mat,
    Prev_trans,
    Prev_phys,
    Curr_geom,
    Curr_mat,
    Curr_trans,
    Curr_phys
) ->
    Patches = [],
    Patches@1 = case Prev_trans /= Curr_trans of
        true ->
            [{update_transform, Id, Curr_trans} | Patches];

        false ->
            Patches
    end,
    Patches@2 = case Prev_mat /= Curr_mat of
        true ->
            [{update_material, Id, Curr_mat} | Patches@1];

        false ->
            Patches@1
    end,
    Patches@3 = case Prev_geom /= Curr_geom of
        true ->
            [{update_geometry, Id, Curr_geom} | Patches@2];

        false ->
            Patches@2
    end,
    Patches@4 = case Prev_phys /= Curr_phys of
        true ->
            [{update_physics, Id, Curr_phys} | Patches@3];

        false ->
            Patches@3
    end,
    Patches@4.

-file("src/tiramisu/scene.gleam", 583).
?DOC(" Compare InstancedMesh fields using accumulator pattern\n").
-spec compare_instanced_mesh_fields(
    ASVM,
    tiramisu@geometry:geometry(),
    tiramisu@material:material(),
    list(tiramisu@transform:transform()),
    tiramisu@geometry:geometry(),
    tiramisu@material:material(),
    list(tiramisu@transform:transform())
) -> list(patch(ASVM)).
compare_instanced_mesh_fields(
    Id,
    Prev_geom,
    Prev_mat,
    Prev_instances,
    Curr_geom,
    Curr_mat,
    Curr_instances
) ->
    Patches = [],
    Patches@1 = case Prev_mat /= Curr_mat of
        true ->
            [{update_material, Id, Curr_mat} | Patches];

        false ->
            Patches
    end,
    Patches@2 = case Prev_geom /= Curr_geom of
        true ->
            [{update_geometry, Id, Curr_geom} | Patches@1];

        false ->
            Patches@1
    end,
    Patches@3 = case Prev_instances /= Curr_instances of
        true ->
            [{update_instances, Id, Curr_instances} | Patches@2];

        false ->
            Patches@2
    end,
    Patches@3.

-file("src/tiramisu/scene.gleam", 613).
?DOC(" Compare Light fields using accumulator pattern\n").
-spec compare_light_fields(
    ASVR,
    tiramisu@light:light(),
    tiramisu@transform:transform(),
    tiramisu@light:light(),
    tiramisu@transform:transform()
) -> list(patch(ASVR)).
compare_light_fields(Id, Prev_light, Prev_trans, Curr_light, Curr_trans) ->
    Patches = [],
    Patches@1 = case Prev_trans /= Curr_trans of
        true ->
            [{update_transform, Id, Curr_trans} | Patches];

        false ->
            Patches
    end,
    Patches@2 = case Prev_light /= Curr_light of
        true ->
            [{update_light, Id, Curr_light} | Patches@1];

        false ->
            Patches@1
    end,
    Patches@2.

-file("src/tiramisu/scene.gleam", 636).
?DOC(" Compare LOD fields using accumulator pattern\n").
-spec compare_lod_fields(
    ASVU,
    list(l_o_d_level(ASVU)),
    tiramisu@transform:transform(),
    list(l_o_d_level(ASVU)),
    tiramisu@transform:transform()
) -> list(patch(ASVU)).
compare_lod_fields(Id, Prev_levels, Prev_trans, Curr_levels, Curr_trans) ->
    Patches = [],
    Patches@1 = case Prev_trans /= Curr_trans of
        true ->
            [{update_transform, Id, Curr_trans} | Patches];

        false ->
            Patches
    end,
    Patches@2 = case Prev_levels /= Curr_levels of
        true ->
            [{update_l_o_d_levels, Id, Curr_levels} | Patches@1];

        false ->
            Patches@1
    end,
    Patches@2.

-file("src/tiramisu/scene.gleam", 659).
?DOC(" Compare Camera fields using accumulator pattern\n").
-spec compare_camera_fields(
    ASWB,
    tiramisu@camera:camera(),
    tiramisu@transform:transform(),
    gleam@option:option(vec@vec3:vec3(float())),
    boolean(),
    gleam@option:option({integer(), integer(), integer(), integer()}),
    tiramisu@camera:camera(),
    tiramisu@transform:transform(),
    gleam@option:option(vec@vec3:vec3(float())),
    boolean(),
    gleam@option:option({integer(), integer(), integer(), integer()})
) -> list(patch(ASWB)).
compare_camera_fields(
    Id,
    Prev_cam,
    Prev_trans,
    Prev_look_at,
    Prev_active,
    Prev_viewport,
    Curr_cam,
    Curr_trans,
    Curr_look_at,
    Curr_active,
    Curr_viewport
) ->
    Patches = [],
    Patches@1 = case Prev_trans /= Curr_trans of
        true ->
            [{update_transform, Id, Curr_trans} | Patches];

        false ->
            Patches
    end,
    Patches@2 = case ((Prev_cam /= Curr_cam) orelse (Prev_look_at /= Curr_look_at))
    orelse (Prev_viewport /= Curr_viewport) of
        true ->
            [{update_camera, Id, Curr_cam, Curr_look_at} | Patches@1];

        false ->
            Patches@1
    end,
    Patches@3 = case {Prev_active, Curr_active} of
        {false, true} ->
            [{set_active_camera, Id} | Patches@2];

        {_, _} ->
            Patches@2
    end,
    Patches@3.

-file("src/tiramisu/scene.gleam", 699).
?DOC(" Compare Model3D fields using accumulator pattern\n").
-spec compare_model3d_fields(
    ASWK,
    tiramisu@transform:transform(),
    gleam@option:option(tiramisu@object3d:animation_playback()),
    gleam@option:option(tiramisu@physics:rigid_body()),
    tiramisu@transform:transform(),
    gleam@option:option(tiramisu@object3d:animation_playback()),
    gleam@option:option(tiramisu@physics:rigid_body())
) -> list(patch(ASWK)).
compare_model3d_fields(
    Id,
    Prev_trans,
    Prev_anim,
    Prev_phys,
    Curr_trans,
    Curr_anim,
    Curr_phys
) ->
    Patches = [],
    Patches@1 = case Prev_trans /= Curr_trans of
        true ->
            [{update_transform, Id, Curr_trans} | Patches];

        false ->
            Patches
    end,
    Patches@2 = case Prev_anim /= Curr_anim of
        true ->
            [{update_animation, Id, Curr_anim} | Patches@1];

        false ->
            Patches@1
    end,
    Patches@3 = case Prev_phys /= Curr_phys of
        true ->
            [{update_physics, Id, Curr_phys} | Patches@2];

        false ->
            Patches@2
    end,
    Patches@3.

-file("src/tiramisu/scene.gleam", 729).
?DOC(" Compare Particles fields using accumulator pattern\n").
-spec compare_particle_fields(
    ASWR,
    tiramisu@particle_emitter:particle_emitter(),
    tiramisu@transform:transform(),
    boolean(),
    tiramisu@particle_emitter:particle_emitter(),
    tiramisu@transform:transform(),
    boolean()
) -> list(patch(ASWR)).
compare_particle_fields(
    Id,
    Prev_emitter,
    Prev_trans,
    Prev_active,
    Curr_emitter,
    Curr_trans,
    Curr_active
) ->
    Patches = [],
    Patches@1 = case Prev_trans /= Curr_trans of
        true ->
            [{update_transform, Id, Curr_trans} | Patches];

        false ->
            Patches
    end,
    Patches@2 = case Prev_emitter /= Curr_emitter of
        true ->
            [{update_particle_emitter, Id, Curr_emitter} | Patches@1];

        false ->
            Patches@1
    end,
    Patches@3 = case Prev_active /= Curr_active of
        true ->
            [{update_particle_active, Id, Curr_active} | Patches@2];

        false ->
            Patches@2
    end,
    Patches@3.

-file("src/tiramisu/scene.gleam", 446).
?DOC(
    " Detailed comparison of node properties (called only when nodes differ)\n"
    " Uses accumulator pattern to avoid empty list allocations\n"
).
-spec compare_nodes_detailed(ASVC, node_(ASVC), node_(ASVC)) -> list(patch(ASVC)).
compare_nodes_detailed(Id, Prev, Curr) ->
    case {Prev, Curr} of
        {{mesh, _, Prev_geom, Prev_mat, Prev_trans, Prev_phys},
            {mesh, _, Curr_geom, Curr_mat, Curr_trans, Curr_phys}} ->
            compare_mesh_fields(
                Id,
                Prev_geom,
                Prev_mat,
                Prev_trans,
                Prev_phys,
                Curr_geom,
                Curr_mat,
                Curr_trans,
                Curr_phys
            );

        {{instanced_mesh, _, Prev_geom@1, Prev_mat@1, Prev_instances},
            {instanced_mesh, _, Curr_geom@1, Curr_mat@1, Curr_instances}} ->
            compare_instanced_mesh_fields(
                Id,
                Prev_geom@1,
                Prev_mat@1,
                Prev_instances,
                Curr_geom@1,
                Curr_mat@1,
                Curr_instances
            );

        {{light, _, Prev_light, Prev_trans@1},
            {light, _, Curr_light, Curr_trans@1}} ->
            compare_light_fields(
                Id,
                Prev_light,
                Prev_trans@1,
                Curr_light,
                Curr_trans@1
            );

        {{group, _, Prev_trans@2, _}, {group, _, Curr_trans@2, _}} ->
            case Prev_trans@2 /= Curr_trans@2 of
                true ->
                    [{update_transform, Id, Curr_trans@2}];

                false ->
                    []
            end;

        {{camera,
                _,
                Prev_cam,
                Prev_trans@3,
                Prev_look_at,
                Prev_active,
                Prev_viewport},
            {camera,
                _,
                Curr_cam,
                Curr_trans@3,
                Curr_look_at,
                Curr_active,
                Curr_viewport}} ->
            compare_camera_fields(
                Id,
                Prev_cam,
                Prev_trans@3,
                Prev_look_at,
                Prev_active,
                Prev_viewport,
                Curr_cam,
                Curr_trans@3,
                Curr_look_at,
                Curr_active,
                Curr_viewport
            );

        {{l_o_d, _, Prev_levels, Prev_trans@4},
            {l_o_d, _, Curr_levels, Curr_trans@4}} ->
            compare_lod_fields(
                Id,
                Prev_levels,
                Prev_trans@4,
                Curr_levels,
                Curr_trans@4
            );

        {{model3_d, _, _, Prev_trans@5, Prev_anim, Prev_phys@1},
            {model3_d, _, _, Curr_trans@5, Curr_anim, Curr_phys@1}} ->
            compare_model3d_fields(
                Id,
                Prev_trans@5,
                Prev_anim,
                Prev_phys@1,
                Curr_trans@5,
                Curr_anim,
                Curr_phys@1
            );

        {{audio, _, Prev_audio}, {audio, _, Curr_audio}} ->
            case Prev_audio /= Curr_audio of
                true ->
                    [{update_audio, Id, Curr_audio}];

                false ->
                    []
            end;

        {{particles, _, Prev_emitter, Prev_trans@6, Prev_active@1},
            {particles, _, Curr_emitter, Curr_trans@6, Curr_active@1}} ->
            compare_particle_fields(
                Id,
                Prev_emitter,
                Prev_trans@6,
                Prev_active@1,
                Curr_emitter,
                Curr_trans@6,
                Curr_active@1
            );

        {_, _} ->
            []
    end.

-file("src/tiramisu/scene.gleam", 437).
-spec compare_nodes(ASUX, node_(ASUX), node_(ASUX)) -> list(patch(ASUX)).
compare_nodes(Id, Prev, Curr) ->
    case Prev =:= Curr of
        true ->
            [];

        false ->
            compare_nodes_detailed(Id, Prev, Curr)
    end.

-file("src/tiramisu/scene.gleam", 243).
?DOC(false).
-spec diff(list(node_(ASTM)), list(node_(ASTM))) -> list(patch(ASTM)).
diff(Previous, Current) ->
    Prev_dict = flatten_scene(Previous),
    Curr_dict = flatten_scene(Current),
    Prev_size = maps:size(Prev_dict),
    Curr_size = maps:size(Curr_dict),
    case (Prev_size =:= 0) andalso (Curr_size =:= 0) of
        true ->
            [];

        false ->
            Prev_ids = maps:keys(Prev_dict),
            Curr_ids = maps:keys(Curr_dict),
            Prev_id_set = gleam@set:from_list(Prev_ids),
            Curr_id_set = gleam@set:from_list(Curr_ids),
            Removals = begin
                _pipe = gleam@list:filter(
                    Prev_ids,
                    fun(Id) -> not gleam@set:contains(Curr_id_set, Id) end
                ),
                gleam@list:map(_pipe, fun(Id@1) -> {remove_node, Id@1} end)
            end,
            {Parent_changed_ids, Same_parent_ids} = begin
                _pipe@1 = gleam@list:filter(
                    Curr_ids,
                    fun(Id@2) -> gleam@set:contains(Prev_id_set, Id@2) end
                ),
                gleam@list:partition(
                    _pipe@1,
                    fun(Id@3) ->
                        case {gleam_stdlib:map_get(Prev_dict, Id@3),
                            gleam_stdlib:map_get(Curr_dict, Id@3)} of
                            {{ok, {node_with_parent, _, Prev_parent}},
                                {ok, {node_with_parent, _, Curr_parent}}} ->
                                Prev_parent /= Curr_parent;

                            {_, _} ->
                                false
                        end
                    end
                )
            end,
            Parent_change_removals = gleam@list:map(
                Parent_changed_ids,
                fun(Id@4) -> {remove_node, Id@4} end
            ),
            Parent_change_additions = gleam@list:filter_map(
                Parent_changed_ids,
                fun(Id@5) -> case gleam_stdlib:map_get(Curr_dict, Id@5) of
                        {ok, {node_with_parent, Node, Parent_id}} ->
                            {ok, {add_node, Id@5, Node, Parent_id}};

                        {error, _} ->
                            {error, nil}
                    end end
            ),
            Additions = begin
                _pipe@2 = gleam@list:filter(
                    Curr_ids,
                    fun(Id@6) -> not gleam@set:contains(Prev_id_set, Id@6) end
                ),
                _pipe@3 = gleam@list:filter_map(
                    _pipe@2,
                    fun(Id@7) -> case gleam_stdlib:map_get(Curr_dict, Id@7) of
                            {ok, {node_with_parent, Node@1, Parent_id@1}} ->
                                {ok, {add_node, Id@7, Node@1, Parent_id@1}};

                            {error, _} ->
                                {error, nil}
                        end end
                ),
                _pipe@4 = lists:append(_pipe@3, Parent_change_additions),
                sort_patches_by_hierarchy(_pipe@4, Curr_dict)
            end,
            Updates = gleam@list:flat_map(
                Same_parent_ids,
                fun(Id@8) ->
                    case {gleam_stdlib:map_get(Prev_dict, Id@8),
                        gleam_stdlib:map_get(Curr_dict, Id@8)} of
                        {{ok, {node_with_parent, Prev_node, _}},
                            {ok, {node_with_parent, Curr_node, _}}} ->
                            compare_nodes(Id@8, Prev_node, Curr_node);

                        {_, _} ->
                            []
                    end
                end
            ),
            batch_patches(Removals, Parent_change_removals, Updates, Additions)
    end.
