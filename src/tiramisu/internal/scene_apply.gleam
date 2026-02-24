import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import gleam/string

import savoiardi

import tiramisu/internal/registry.{type Registry}
import tiramisu/internal/render_loop.{type RenderLoop}
import tiramisu/internal/scene_patch.{type ScenePatch}
import tiramisu/transform.{type Transform}

import vec/vec3

/// Apply a list of patches to the Three.js scene graph.
/// Returns the updated Registry after all patches are applied.
///
/// Side effects (Three.js object creation, RenderLoop updates) happen during
/// the fold. Async operations (model loading) capture on_async and dispatch
/// transforms when they complete.
pub fn apply_patches(
  registry: Registry,
  loop: RenderLoop,
  patches: List(ScenePatch),
  on_async: fn(fn(Registry) -> Registry) -> Nil,
) -> Registry {
  use registry, patch <- list.fold(patches, registry)
  apply_patch(registry, loop, patch, on_async)
}

fn apply_patch(
  registry: Registry,
  loop: RenderLoop,
  patch: ScenePatch,
  on_async: fn(fn(Registry) -> Registry) -> Nil,
) -> Registry {
  case patch {
    scene_patch.CreateMesh(
      id:,
      parent_id:,
      geometry:,
      src:,
      material_type:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe:,
      emissive:,
      emissive_intensity:,
      side:,
      color_map:,
      normal_map:,
      ao_map:,
      roughness_map:,
      metalness_map:,
      displacement_map:,
      displacement_scale:,
      displacement_bias:,
      shininess:,
      alpha_test:,
      transparent:,
      transform:,
      visible:,
      cast_shadow:,
      receive_shadow:,
    ) -> {
      case src {
        // External model — load asynchronously (fire-and-forget)
        option.Some(url) -> {
          load_model(
            on_async,
            id,
            parent_id,
            url,
            transform,
            option.unwrap(visible, True),
            option.unwrap(cast_shadow, False),
            option.unwrap(receive_shadow, False),
          )
          registry
        }
        option.None -> {
          let color_int =
            color
            |> option.to_result(Nil)
            |> result.try(parse_color)
            |> result.unwrap(0xffffff)
          let emissive_int =
            emissive
            |> option.to_result(Nil)
            |> result.try(parse_color)
            |> result.unwrap(0x000000)
          case geometry |> option.to_result(Nil) |> result.try(parse_geometry) {
            Ok(geometry) -> {
              let material =
                create_material_by_type(
                  material_type,
                  color_int,
                  emissive_int,
                  metalness,
                  roughness,
                  opacity,
                  emissive_intensity,
                  displacement_scale,
                  displacement_bias,
                  shininess,
                  alpha_test,
                  transparent,
                )
              savoiardi.update_material_wireframe(
                material,
                option.unwrap(wireframe, False),
              )
              savoiardi.update_material_side(
                material,
                parse_material_side(option.unwrap(side, "front")),
              )
              let mesh = savoiardi.create_mesh(geometry, material)
              // Register FIRST so set_transform/set_visible can find the object
              let reg =
                registry.register_and_add_object(
                  registry,
                  id,
                  mesh,
                  parent_id,
                  registry.MeshObject,
                )
              registry.set_transform(reg, id, transform)
              registry.set_visible(reg, id, option.unwrap(visible, True))
              registry.set_mesh_shadow(
                reg,
                id,
                option.unwrap(cast_shadow, False),
                option.unwrap(receive_shadow, False),
              )
              // Load texture maps asynchronously (fire-and-forget)
              let _ =
                load_material_textures(
                  material,
                  color_map,
                  normal_map,
                  ao_map,
                  roughness_map,
                  metalness_map,
                  displacement_map,
                )
              reg
            }
            Error(Nil) -> registry
          }
        }
      }
    }

    scene_patch.CreateCamera(
      id:,
      parent_id:,
      camera_type:,
      fov:,
      near:,
      far:,
      transform:,
      active:,
    ) -> {
      let near_val = option.unwrap(near, 0.1)
      let far_val = option.unwrap(far, 1000.0)
      let camera = case option.unwrap(camera_type, "perspective") {
        "orthographic" ->
          savoiardi.create_orthographic_camera(
            -10.0,
            10.0,
            10.0,
            -10.0,
            near_val,
            far_val,
          )
        // Default to perspective
        _ ->
          savoiardi.create_perspective_camera(
            option.unwrap(fov, 75.0),
            registry.get_renderer_aspect_ratio(registry),
            near_val,
            far_val,
          )
      }
      // Register FIRST so set_transform can find the object
      let reg =
        registry.register_camera(
          registry,
          id,
          camera,
          parent_id,
          option.unwrap(active, False),
        )
      registry.set_transform(reg, id, transform)
      // Sync active camera to render loop
      sync_active_camera(reg, loop)
      reg
    }

    scene_patch.CreateLight(
      id:,
      parent_id:,
      light_type:,
      color:,
      intensity:,
      transform:,
      cast_shadow:,
    ) -> {
      case parse_color(option.unwrap(color, "#ffffff")) {
        Ok(color_int) -> {
          let intensity_val = option.unwrap(intensity, 1.0)
          let casts_shadow = option.unwrap(cast_shadow, False)
          let light = case option.unwrap(light_type, "point") {
            "directional" -> {
              let light =
                savoiardi.create_directional_light(color_int, intensity_val)
              case casts_shadow {
                True -> {
                  savoiardi.set_light_cast_shadow(light, True)
                  savoiardi.configure_shadow(
                    light,
                    savoiardi.ShadowConfig(
                      resolution: 1024,
                      bias: -0.0001,
                      normal_bias: 0.0,
                    ),
                  )
                  savoiardi.configure_directional_shadow_camera(
                    light,
                    savoiardi.DirectionalShadowConfig(
                      camera_left: -10.0,
                      camera_right: 10.0,
                      camera_top: 10.0,
                      camera_bottom: -10.0,
                      camera_near: 0.5,
                      camera_far: 500.0,
                    ),
                  )
                }
                False -> Nil
              }
              savoiardi.light_to_object3d(light)
            }
            "point" -> {
              let light =
                savoiardi.create_point_light(color_int, intensity_val, 0.0)
              case casts_shadow {
                True -> {
                  savoiardi.set_light_cast_shadow(light, True)
                  savoiardi.configure_shadow(
                    light,
                    savoiardi.ShadowConfig(
                      resolution: 1024,
                      bias: -0.0001,
                      normal_bias: 0.0,
                    ),
                  )
                }
                False -> Nil
              }
              savoiardi.light_to_object3d(light)
            }
            // Default to ambient
            _ -> {
              savoiardi.create_ambient_light(color_int, intensity_val)
              |> savoiardi.light_to_object3d
            }
          }
          // Register FIRST so set_transform can find the object
          let reg =
            registry.register_and_add_object(
              registry,
              id,
              light,
              parent_id,
              registry.LightObject,
            )
          registry.set_transform(reg, id, transform)
          reg
        }
        Error(Nil) -> registry
      }
    }

    scene_patch.CreateGroup(id:, parent_id:, transform:, visible:) -> {
      let group = savoiardi.create_group()
      // Register FIRST so set_transform/set_visible can find the object
      let reg =
        registry.register_and_add_object(
          registry,
          id,
          group,
          parent_id,
          registry.GroupObject,
        )
      registry.set_transform(reg, id, transform)
      registry.set_visible(reg, id, option.unwrap(visible, True))
      reg
    }

    scene_patch.CreateAudio(
      id:,
      parent_id:,
      src:,
      volume:,
      loop: audio_loop,
      playing:,
      playback_rate:,
      detune:,
    ) -> {
      let #(reg, listener) = get_or_create_listener(registry)
      let audio = savoiardi.create_audio(listener)
      savoiardi.set_audio_volume(audio, option.unwrap(volume, 1.0))
      savoiardi.set_audio_loop(audio, option.unwrap(audio_loop, False))
      savoiardi.set_audio_playback_rate(
        audio,
        option.unwrap(playback_rate, 1.0),
      )
      savoiardi.set_audio_detune(audio, option.unwrap(detune, 0.0))
      load_and_play(audio, src, option.unwrap(playing, False))
      let reg = registry.register_audio(reg, parent_id, id, audio)
      reg
    }

    scene_patch.CreatePositionalAudio(
      id:,
      parent_id:,
      src:,
      volume:,
      loop: audio_loop,
      playing:,
      playback_rate:,
      detune:,
      transform:,
      ref_distance:,
      max_distance:,
      rolloff_factor:,
    ) -> {
      let #(reg, listener) = get_or_create_listener(registry)
      let pos_audio = savoiardi.create_positional_audio(listener)
      savoiardi.set_positional_audio_volume(
        pos_audio,
        option.unwrap(volume, 1.0),
      )
      savoiardi.set_positional_audio_loop(
        pos_audio,
        option.unwrap(audio_loop, False),
      )
      savoiardi.set_positional_audio_playback_rate(
        pos_audio,
        option.unwrap(playback_rate, 1.0),
      )
      savoiardi.set_positional_audio_detune(
        pos_audio,
        option.unwrap(detune, 0.0),
      )
      savoiardi.set_ref_distance(pos_audio, option.unwrap(ref_distance, 1.0))
      savoiardi.set_max_distance(pos_audio, option.unwrap(max_distance, 100.0))
      savoiardi.set_rolloff_factor(
        pos_audio,
        option.unwrap(rolloff_factor, 1.0),
      )
      load_and_play_positional(pos_audio, src, option.unwrap(playing, False))
      // Register FIRST so set_transform can find the object
      let reg =
        registry.register_positional_audio(reg, parent_id, id, pos_audio)
      registry.set_transform(reg, id, transform)
      reg
    }

    scene_patch.CreateDebug(
      id:,
      parent_id:,
      debug_type:,
      size:,
      divisions:,
      color:,
      transform:,
    ) -> {
      case parse_color(option.unwrap(color, "#888888")) {
        Ok(color_int) -> {
          let size_val = option.unwrap(size, 5.0)
          let divisions_val = option.unwrap(divisions, 10)
          let object = case option.unwrap(debug_type, "axes") {
            "axes" -> savoiardi.create_axes_helper(size_val)
            "grid" ->
              savoiardi.create_grid_helper(size_val, divisions_val, color_int)
            // Default to axes
            _ -> savoiardi.create_axes_helper(size_val)
          }
          // Register FIRST so set_transform can find the object
          let reg =
            registry.register_and_add_object(
              registry,
              id,
              object,
              parent_id,
              registry.DebugObject,
            )
          registry.set_transform(reg, id, transform)
          reg
        }
        Error(Nil) -> registry
      }
    }

    // -- Updates ----------------------------------------------------------------
    scene_patch.UpdateTransform(id:, transform:) -> {
      registry.set_transform(registry, id, transform)
      registry
    }

    scene_patch.UpdateMeshGeometry(id:, geometry:) -> {
      let _ =
        registry.get_object(registry, id)
        |> result.map(fn(object) {
          savoiardi.get_object_geometry(object)
          |> savoiardi.dispose_geometry

          geometry
          |> option.to_result(Nil)
          |> result.try(parse_geometry)
          |> result.map(savoiardi.set_object_geometry(object, _))
        })
      registry
    }

    scene_patch.UpdateMeshSrc(id:, src:) -> {
      case src {
        option.Some(url) -> set_model(on_async, id, url)
        option.None -> {
          let _ =
            registry.get_object(registry, id)
            |> result.map(savoiardi.get_object_geometry)
            |> result.map(savoiardi.dispose_geometry)
          Nil
        }
      }
      registry
    }

    scene_patch.UpdateMeshMaterial(
      id:,
      material_type:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe:,
      emissive:,
      emissive_intensity:,
      side:,
      color_map:,
      normal_map:,
      ao_map:,
      roughness_map:,
      metalness_map:,
      displacement_map:,
      displacement_scale:,
      displacement_bias:,
      shininess:,
      alpha_test:,
      transparent:,
    ) -> {
      let _ =
        registry.get_object(registry, id)
        |> result.map(fn(object) {
          let color_int =
            color
            |> option.to_result(Nil)
            |> result.try(parse_color)
            |> result.unwrap(0xffffff)
          let emissive_int =
            emissive
            |> option.to_result(Nil)
            |> result.try(parse_color)
            |> result.unwrap(0x000000)
          let new_material =
            create_material_by_type(
              material_type,
              color_int,
              emissive_int,
              metalness,
              roughness,
              opacity,
              emissive_intensity,
              displacement_scale,
              displacement_bias,
              shininess,
              alpha_test,
              transparent,
            )
          savoiardi.update_material_wireframe(
            new_material,
            option.unwrap(wireframe, False),
          )
          savoiardi.update_material_side(
            new_material,
            parse_material_side(option.unwrap(side, "front")),
          )
          // Dispose old material and replace
          let old_material = savoiardi.get_object_material(object)
          savoiardi.dispose_material(old_material)
          savoiardi.set_object_material(object, new_material)
          // Load texture maps asynchronously (fire-and-forget)
          let _ =
            load_material_textures(
              new_material,
              color_map,
              normal_map,
              ao_map,
              roughness_map,
              metalness_map,
              displacement_map,
            )
          Nil
        })

      registry
    }

    scene_patch.UpdateMeshShadow(id:, cast_shadow:, receive_shadow:) -> {
      registry.set_mesh_shadow(
        registry,
        id,
        option.unwrap(cast_shadow, False),
        option.unwrap(receive_shadow, False),
      )
      registry
    }

    scene_patch.UpdateMeshVisibility(id:, visible:) -> {
      registry.set_visible(registry, id, option.unwrap(visible, True))
      registry
    }

    scene_patch.UpdateCameraProps(id:, fov:, near:, far:, ..) -> {
      let _ =
        registry.get_camera(registry, id)
        |> result.map(fn(camera) {
          savoiardi.set_perspective_camera_params(
            camera,
            option.unwrap(fov, 75.0),
            registry.get_renderer_aspect_ratio(registry),
            option.unwrap(near, 0.1),
            option.unwrap(far, 1000.0),
          )
          savoiardi.update_camera_projection_matrix(camera)
        })
      registry
    }

    scene_patch.UpdateCameraActive(id:, active:) -> {
      let reg =
        registry.set_camera_active(registry, id, option.unwrap(active, False))
      // Sync active camera to render loop
      sync_active_camera(reg, loop)
      reg
    }

    scene_patch.UpdateLightProps(id:, color:, intensity:, ..) -> {
      let _ = {
        use light <- result.try(
          registry.get_light(registry, id) |> option.to_result(Nil),
        )
        use color_int <- result.try(
          parse_color(option.unwrap(color, "#ffffff")),
        )
        savoiardi.update_light_color(light, color_int)
        savoiardi.update_light_intensity(light, option.unwrap(intensity, 1.0))
        Ok(Nil)
      }
      registry
    }

    scene_patch.UpdateAudioProps(
      id:,
      src: _,
      volume:,
      loop: audio_loop,
      playing:,
      playback_rate:,
      detune:,
    ) -> {
      let _ =
        registry.get_audio(registry, id)
        |> result.map(fn(audio) {
          savoiardi.set_audio_volume(audio, option.unwrap(volume, 1.0))
          savoiardi.set_audio_loop(audio, option.unwrap(audio_loop, False))
          savoiardi.set_audio_playback_rate(
            audio,
            option.unwrap(playback_rate, 1.0),
          )
          savoiardi.set_audio_detune(audio, option.unwrap(detune, 0.0))
          update_audio_playback(audio, option.unwrap(playing, False))
        })
      registry
    }

    scene_patch.UpdatePositionalAudio(
      id:,
      src: _,
      volume:,
      loop: audio_loop,
      playing:,
      playback_rate:,
      detune:,
      transform:,
      ref_distance:,
      max_distance:,
      rolloff_factor:,
    ) -> {
      registry.set_transform(registry, id, transform)
      let _ =
        registry.get_positional_audio(registry, id)
        |> result.map(fn(pos_audio) {
          savoiardi.set_positional_audio_volume(
            pos_audio,
            option.unwrap(volume, 1.0),
          )
          savoiardi.set_positional_audio_loop(
            pos_audio,
            option.unwrap(audio_loop, False),
          )
          savoiardi.set_positional_audio_playback_rate(
            pos_audio,
            option.unwrap(playback_rate, 1.0),
          )
          savoiardi.set_positional_audio_detune(
            pos_audio,
            option.unwrap(detune, 0.0),
          )
          savoiardi.set_ref_distance(
            pos_audio,
            option.unwrap(ref_distance, 1.0),
          )
          savoiardi.set_max_distance(
            pos_audio,
            option.unwrap(max_distance, 100.0),
          )
          savoiardi.set_rolloff_factor(
            pos_audio,
            option.unwrap(rolloff_factor, 1.0),
          )
          update_positional_audio_playback(
            pos_audio,
            option.unwrap(playing, False),
          )
        })
      registry
    }

    scene_patch.UpdateGroupVisibility(id:, visible:) -> {
      registry.set_visible(registry, id, option.unwrap(visible, True))
      registry
    }

    // -- InstancedMesh creation -------------------------------------------------
    scene_patch.CreateInstancedMesh(
      id:,
      parent_id:,
      geometry:,
      material_type:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe:,
      transparent:,
      instances:,
      transform:,
      visible:,
      cast_shadow:,
      receive_shadow:,
    ) -> {
      let instances_str = option.unwrap(instances, "")
      case
        geometry |> option.to_result(Nil) |> result.try(parse_geometry),
        color |> option.to_result(Nil) |> result.try(parse_color)
      {
        Ok(geom), Ok(color_int) -> {
          let material =
            create_material_by_type(
              material_type,
              color_int,
              0,
              metalness,
              roughness,
              opacity,
              option.Some(0.0),
              option.Some(1.0),
              option.Some(0.0),
              option.Some(30.0),
              option.Some(0.0),
              transparent,
            )
          savoiardi.update_material_wireframe(
            material,
            option.unwrap(wireframe, False),
          )
          let count = parse_instance_count(instances_str)
          // Create with generous capacity so count changes don't require recreation.
          let capacity = int.max(count * 4, 64)
          let instanced =
            savoiardi.create_instanced_mesh(geom, material, capacity)
          // Set the actual rendered count (capacity > count).
          registry.set_instanced_mesh_count(instanced, count)
          let transforms = parse_instance_transforms(instances_str)
          savoiardi.update_instanced_mesh_transforms(instanced, transforms)
          let object = savoiardi.instanced_mesh_to_object3d(instanced)
          // Register FIRST so set_transform/set_visible/set_mesh_shadow can find it
          let reg =
            registry.register_and_add_object(
              registry,
              id,
              object,
              parent_id,
              registry.InstancedMeshObject,
            )
          registry.set_transform(reg, id, transform)
          registry.set_visible(reg, id, option.unwrap(visible, True))
          registry.set_mesh_shadow(
            reg,
            id,
            option.unwrap(cast_shadow, False),
            option.unwrap(receive_shadow, False),
          )
          reg
        }
        _, _ -> registry
      }
    }

    scene_patch.UpdateInstancedMeshInstances(id:, instances:) -> {
      let _ =
        registry.get_instanced_mesh(registry, id)
        |> result.map(fn(instanced) {
          let instances_str = option.unwrap(instances, "")
          let count = parse_instance_count(instances_str)
          let transforms = parse_instance_transforms(instances_str)
          savoiardi.update_instanced_mesh_transforms(instanced, transforms)
          // Update the rendered count — Three.js only renders .count instances
          registry.set_instanced_mesh_count(instanced, count)
        })
      registry
    }

    scene_patch.UpdateInstancedMeshMaterial(
      id:,
      material_type:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe:,
      transparent:,
    ) -> {
      let _ = {
        use object <- result.try(registry.get_object(registry, id))
        use color_int <- result.try(
          color |> option.to_result(Nil) |> result.try(parse_color),
        )
        let new_material =
          create_material_by_type(
            material_type,
            color_int,
            0,
            metalness,
            roughness,
            opacity,
            option.Some(0.0),
            option.Some(1.0),
            option.Some(0.0),
            option.Some(30.0),
            option.Some(0.0),
            transparent,
          )
        savoiardi.update_material_wireframe(
          new_material,
          option.unwrap(wireframe, False),
        )
        savoiardi.get_object_material(object)
        |> savoiardi.dispose_material
        savoiardi.set_object_material(object, new_material)
        Ok(Nil)
      }
      registry
    }

    scene_patch.UpdateInstancedMeshVisibility(id:, visible:) -> {
      registry.set_visible(registry, id, option.unwrap(visible, True))
      registry
    }

    scene_patch.UpdateInstancedMeshShadow(id:, cast_shadow:, receive_shadow:) -> {
      registry.set_mesh_shadow(
        registry,
        id,
        option.unwrap(cast_shadow, False),
        option.unwrap(receive_shadow, False),
      )
      registry
    }

    // -- Structure --------------------------------------------------------------
    scene_patch.Remove(id:) -> {
      let reg = registry.remove_object(registry, id)
      // If a camera was removed, sync the active camera
      sync_active_camera(reg, loop)
      reg
    }

    scene_patch.Reparent(id:, new_parent_id:) -> {
      let reg = registry.reparent_object(registry, id, new_parent_id)
      reg
    }
  }
}

// ACTIVE CAMERA SYNC ----------------------------------------------------------

/// Sync the render loop's active camera with the registry's camera state.
fn sync_active_camera(reg: Registry, loop: RenderLoop) -> Nil {
  case registry.find_active_camera(reg) {
    option.Some(camera) -> render_loop.set_active_camera(loop, camera)
    option.None -> render_loop.clear_active_camera(loop)
  }
}

// GEOMETRY PARSING ------------------------------------------------------------

fn parse_geometry(geometry: String) -> Result(savoiardi.Geometry, Nil) {
  case string.split(geometry, ":") {
    [type_str, params_str] -> {
      let params =
        string.split(params_str, ",")
        |> list.map(string.trim)
        |> list.filter_map(float.parse)

      case type_str, params {
        "box", [w, h, d] -> Ok(savoiardi.create_box_geometry(w, h, d))
        "box", [s] -> Ok(savoiardi.create_box_geometry(s, s, s))
        "box", _ -> Ok(savoiardi.create_box_geometry(1.0, 1.0, 1.0))

        "sphere", [r, ws, hs] ->
          Ok(savoiardi.create_sphere_geometry(
            r,
            float.round(ws),
            float.round(hs),
          ))
        "sphere", [r] -> Ok(savoiardi.create_sphere_geometry(r, 32, 16))
        "sphere", _ -> Ok(savoiardi.create_sphere_geometry(1.0, 32, 16))

        "plane", [w, h] -> Ok(savoiardi.create_plane_geometry(w, h, 1, 1))
        "plane", _ -> Ok(savoiardi.create_plane_geometry(1.0, 1.0, 1, 1))

        "cylinder", [rt, rb, h, s] ->
          Ok(savoiardi.create_cylinder_geometry(rt, rb, h, float.round(s)))
        "cylinder", [r, h] ->
          Ok(savoiardi.create_cylinder_geometry(r, r, h, 32))
        "cylinder", _ ->
          Ok(savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32))

        "cone", [r, h, s] ->
          Ok(savoiardi.create_cone_geometry(r, h, float.round(s)))
        "cone", [r, h] -> Ok(savoiardi.create_cone_geometry(r, h, 32))
        "cone", _ -> Ok(savoiardi.create_cone_geometry(1.0, 1.0, 32))

        "torus", [r, t, rs, ts] ->
          Ok(savoiardi.create_torus_geometry(
            r,
            t,
            float.round(rs),
            float.round(ts),
          ))
        "torus", [r, t] -> Ok(savoiardi.create_torus_geometry(r, t, 16, 48))
        "torus", _ -> Ok(savoiardi.create_torus_geometry(1.0, 0.4, 16, 48))

        _, _ -> Error(Nil)
      }
    }
    [type_str] -> {
      case type_str {
        "box" -> Ok(savoiardi.create_box_geometry(1.0, 1.0, 1.0))
        "sphere" -> Ok(savoiardi.create_sphere_geometry(1.0, 32, 16))
        "plane" -> Ok(savoiardi.create_plane_geometry(1.0, 1.0, 1, 1))
        "cylinder" -> Ok(savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32))
        "cone" -> Ok(savoiardi.create_cone_geometry(1.0, 1.0, 32))
        "torus" -> Ok(savoiardi.create_torus_geometry(1.0, 0.4, 16, 48))
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

// COLOR PARSING ---------------------------------------------------------------

fn parse_color(hex: String) -> Result(Int, Nil) {
  let clean = string.replace(hex, "#", "")
  int.base_parse(clean, 16)
}

// AUDIO HELPERS ---------------------------------------------------------------

/// Get or create an AudioListener for the given scene.
/// The listener is stored in the registry and attached to the
/// active camera so positional audio panning works correctly.
/// Returns the updated registry and the listener.
fn get_or_create_listener(reg: Registry) -> #(Registry, savoiardi.AudioListener) {
  case registry.get_audio_listener(reg) {
    Ok(listener) -> #(reg, listener)
    Error(Nil) -> {
      let listener = savoiardi.create_audio_listener()
      let reg = registry.store_audio_listener(reg, listener)
      registry.attach_listener_to_camera(reg, listener)
      #(reg, listener)
    }
  }
}

/// Load an audio buffer from a URL and play the global Audio when ready.
fn load_and_play(
  audio: savoiardi.Audio,
  src: option.Option(String),
  should_play: Bool,
) -> Nil {
  case src {
    option.None -> Nil
    option.Some(url) -> {
      let _ = {
        use result <- promise.map(savoiardi.load_audio(url))
        use buffer <- result.map(result)

        savoiardi.set_audio_buffer(audio, buffer)

        case should_play {
          True -> {
            savoiardi.resume_audio_context()
            savoiardi.play_audio(audio)
          }
          False -> Nil
        }
      }
      Nil
    }
  }
}

/// Load an audio buffer and play a PositionalAudio when ready.
fn load_and_play_positional(
  audio: savoiardi.PositionalAudio,
  src: option.Option(String),
  should_play: Bool,
) -> Nil {
  case src {
    option.None -> Nil
    option.Some(url) -> {
      let _ = {
        use result <- promise.map(savoiardi.load_audio(url))
        use buffer <- result.map(result)

        savoiardi.set_positional_audio_buffer(audio, buffer)

        case should_play {
          True -> {
            savoiardi.resume_audio_context()
            savoiardi.play_positional_audio(audio)
          }
          False -> Nil
        }
      }
      Nil
    }
  }
}

/// Update playback state for a global Audio object.
fn update_audio_playback(audio: savoiardi.Audio, playing: Bool) -> Nil {
  case playing, savoiardi.is_audio_playing(audio) {
    True, False -> {
      savoiardi.resume_audio_context()
      savoiardi.play_audio(audio)
    }
    False, True -> {
      savoiardi.pause_audio(audio)
    }
    False, False | True, True -> Nil
  }
}

/// Update playback state for a PositionalAudio object.
fn update_positional_audio_playback(
  audio: savoiardi.PositionalAudio,
  playing: Bool,
) -> Nil {
  case playing, savoiardi.is_positional_audio_playing(audio) {
    True, False -> {
      savoiardi.resume_audio_context()
      savoiardi.play_positional_audio(audio)
    }
    False, True -> {
      savoiardi.pause_positional_audio(audio)
    }
    False, False | True, True -> Nil
  }
}

// MODEL LOADING ---------------------------------------------------------------

/// Load an external 3D model and add it to the scene.
/// Fire-and-forget — dispatches a RegistryTransform via on_async when the
/// model loads.
fn load_model(
  on_async: fn(fn(Registry) -> Registry) -> Nil,
  id: String,
  parent_id: String,
  src: String,
  transform: Transform,
  visible: Bool,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> Nil {
  let register_loaded =
    register_loaded_model(
      on_async,
      id,
      parent_id,
      transform,
      visible,
      cast_shadow,
      receive_shadow,
    )
  let extension = get_file_extension(src)
  case extension {
    "gltf" | "glb" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_gltf(src))
        case result {
          Ok(data) -> register_loaded(savoiardi.get_gltf_scene(data))
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "fbx" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_fbx(src))
        case result {
          Ok(data) -> register_loaded(savoiardi.get_fbx_scene(data))
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "obj" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_obj(src))
        case result {
          Ok(data) -> register_loaded(data)
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "stl" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_stl(src))
        case result {
          Ok(data) -> {
            let centered = savoiardi.center_geometry(data)
            let material =
              savoiardi.create_standard_material(
                0xcccccc,
                0.5,
                0.5,
                False,
                1.0,
                option.None,
                option.None,
                option.None,
                option.None,
                1.0,
                0.0,
                option.None,
                option.None,
                0x000000,
                1.0,
                0.0,
              )
            let mesh = savoiardi.create_mesh(centered, material)
            register_loaded(mesh)
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    _ -> Nil
  }
}

/// Shared callback for all model loaders: registers the loaded object in the
/// scene and dispatches the model-loaded event.
fn register_loaded_model(
  on_async: fn(fn(Registry) -> Registry) -> Nil,
  id: String,
  parent_id: String,
  transform: Transform,
  visible: Bool,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> fn(savoiardi.Object3D) -> Nil {
  fn(object) {
    on_async(fn(registry) {
      let reg =
        registry.register_and_add_object(
          registry,
          id,
          object,
          parent_id,
          registry.MeshObject,
        )
      registry.set_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
      registry.set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
      reg
    })
    registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
  }
}

/// Replace an existing object's model when the src attribute changes.
/// Fire-and-forget — dispatches a RegistryTransform via on_async when the
/// new model loads.
fn set_model(
  on_async: fn(fn(Registry) -> Registry) -> Nil,
  id: String,
  src: String,
) -> Nil {
  let extension = get_file_extension(src)
  case extension {
    "gltf" | "glb" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_gltf(src))
        case result {
          Ok(data) -> {
            let object = savoiardi.get_gltf_scene(data)
            on_async(fn(reg) { registry.replace_object_model(reg, id, object) })
            registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "fbx" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_fbx(src))
        case result {
          Ok(data) -> {
            let object = savoiardi.get_fbx_scene(data)
            on_async(fn(reg) { registry.replace_object_model(reg, id, object) })
            registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    "obj" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_obj(src))
        case result {
          Ok(data) -> {
            on_async(fn(reg) { registry.replace_object_model(reg, id, data) })
            registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
          }
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    _ -> Nil
  }
}

/// Extract the file extension from a URL, stripping query parameters.
fn get_file_extension(url: String) -> String {
  url
  |> string.split("?")
  |> list.first
  |> result.unwrap(url)
  |> string.split(".")
  |> list.last
  |> result.unwrap("")
  |> string.lowercase
}

// MATERIAL HELPERS ------------------------------------------------------------

/// Create a Three.js material based on the material type string.
/// Colors must be pre-resolved to Int by the caller.
fn create_material_by_type(
  material_type: option.Option(String),
  color: Int,
  emissive: Int,
  metalness: option.Option(Float),
  roughness: option.Option(Float),
  opacity: option.Option(Float),
  emissive_intensity: option.Option(Float),
  displacement_scale: option.Option(Float),
  displacement_bias: option.Option(Float),
  shininess: option.Option(Float),
  alpha_test: option.Option(Float),
  transparent: option.Option(Bool),
) -> savoiardi.Material {
  let opacity_val = option.unwrap(opacity, 1.0)
  let is_transparent = option.unwrap(transparent, False) || opacity_val <. 1.0
  case option.unwrap(material_type, "standard") {
    "basic" ->
      savoiardi.create_basic_material(
        color,
        is_transparent,
        opacity_val,
        option.None,
        savoiardi.FrontSide,
        option.unwrap(alpha_test, 0.0),
        True,
      )
    "phong" ->
      savoiardi.create_phong_material(
        color,
        option.unwrap(shininess, 30.0),
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity_val,
        option.unwrap(alpha_test, 0.0),
      )
    "lambert" ->
      savoiardi.create_lambert_material(
        color,
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity_val,
        option.unwrap(alpha_test, 0.0),
        savoiardi.FrontSide,
      )
    "toon" ->
      savoiardi.create_toon_material(
        color,
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity_val,
        option.unwrap(alpha_test, 0.0),
      )
    // Default to standard (PBR)
    _ ->
      savoiardi.create_standard_material(
        color,
        option.unwrap(metalness, 0.5),
        option.unwrap(roughness, 0.5),
        is_transparent,
        opacity_val,
        option.None,
        option.None,
        option.None,
        option.None,
        option.unwrap(displacement_scale, 1.0),
        option.unwrap(displacement_bias, 0.0),
        option.None,
        option.None,
        emissive,
        option.unwrap(emissive_intensity, 1.0),
        option.unwrap(alpha_test, 0.0),
      )
  }
}

/// Load texture maps from URLs and apply them to a material asynchronously.
fn load_material_textures(
  material: savoiardi.Material,
  color_map: option.Option(String),
  normal_map: option.Option(String),
  ao_map: option.Option(String),
  roughness_map: option.Option(String),
  metalness_map: option.Option(String),
  displacement_map: option.Option(String),
) -> promise.Promise(Result(Nil, Nil)) {
  [
    load_texture_to_material(material, color_map, "map"),
    load_texture_to_material(material, normal_map, "normalMap"),
    load_texture_to_material(material, ao_map, "aoMap"),
    load_texture_to_material(material, roughness_map, "roughnessMap"),
    load_texture_to_material(material, metalness_map, "metalnessMap"),
    load_texture_to_material(material, displacement_map, "displacementMap"),
  ]
  |> promise.await_list()
  |> promise.map(fn(results) {
    case list.any(results, result.is_error) {
      True -> Error(Nil)
      False -> Ok(Nil)
    }
  })
}

/// Load a single texture from a URL and set it on a material property.
fn load_texture_to_material(
  material: savoiardi.Material,
  url: option.Option(String),
  property_name: String,
) -> promise.Promise(Result(Nil, Nil)) {
  case url {
    option.None -> promise.resolve(Error(Nil))
    option.Some(u) -> {
      use result <- promise.map(savoiardi.load_texture(u))
      use texture <- result.map(result)
      savoiardi.set_material_texture(material, property_name, texture)
    }
  }
}

// MATERIAL HELPERS (side conversion) ------------------------------------------

fn parse_material_side(side_str: String) -> savoiardi.MaterialSide {
  case side_str {
    "back" -> savoiardi.BackSide
    "double" -> savoiardi.DoubleSide
    _ -> savoiardi.FrontSide
  }
}

// INSTANCE TRANSFORM PARSING --------------------------------------------------

/// Count the number of instances from the serialized string.
/// Format: "x,y,z,rx,ry,rz,sx,sy,sz|x,y,z,rx,ry,rz,sx,sy,sz|..."
fn parse_instance_count(instances: String) -> Int {
  case instances {
    "" -> 0
    _ ->
      string.split(instances, "|")
      |> list.length
  }
}

/// Parse serialized instance transforms into a list of (position, rotation, scale) tuples.
/// Format: "x,y,z,rx,ry,rz,sx,sy,sz|..."
fn parse_instance_transforms(
  instances: String,
) -> List(#(vec3.Vec3(Float), vec3.Vec3(Float), vec3.Vec3(Float))) {
  case instances {
    "" -> []
    _ ->
      string.split(instances, "|")
      |> list.filter_map(fn(entry) {
        let values =
          string.split(entry, ",")
          |> list.map(string.trim)
          |> list.filter_map(float.parse)
        case values {
          [px, py, pz, rx, ry, rz, sx, sy, sz] ->
            Ok(#(
              vec3.Vec3(px, py, pz),
              vec3.Vec3(rx, ry, rz),
              vec3.Vec3(sx, sy, sz),
            ))
          [px, py, pz, rx, ry, rz] ->
            Ok(#(
              vec3.Vec3(px, py, pz),
              vec3.Vec3(rx, ry, rz),
              vec3.Vec3(1.0, 1.0, 1.0),
            ))
          [px, py, pz] ->
            Ok(#(
              vec3.Vec3(px, py, pz),
              vec3.Vec3(0.0, 0.0, 0.0),
              vec3.Vec3(1.0, 1.0, 1.0),
            ))
          _ -> Error(Nil)
        }
      })
  }
}
