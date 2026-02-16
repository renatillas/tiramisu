//// Pure Gleam patch applier for the scene graph.
////
//// Pattern-matches on ScenePatch variants and calls registry + savoiardi
//// functions. No global state — everything goes through the instance-scoped
//// Registry, updated as a functional fold over the patch list.
////
//// Per-frame mutable state (active camera) is owned by the RenderLoop
//// and updated via targeted setter calls.
////
//// Async operations (model loading) dispatch transforms through on_async,
//// which maps to Lustre's RegistryTransform message dispatch.

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
  list.fold(patches, registry, fn(reg, patch) {
    apply_patch(reg, loop, patch, on_async)
  })
}

fn apply_patch(
  reg: Registry,
  loop: RenderLoop,
  patch: ScenePatch,
  on_async: fn(fn(Registry) -> Registry) -> Nil,
) -> Registry {
  case patch {
    scene_patch.CreateMesh(
      id:,
      parent_id:,
      geometry: geo_spec,
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
      distance:,
    ) -> {
      case src {
        // External model — load asynchronously (fire-and-forget)
        src if src != "" -> {
          load_model(
            on_async,
            id,
            parent_id,
            src,
            transform,
            visible,
            cast_shadow,
            receive_shadow,
          )
          // Return registry unchanged — model will be added via on_async
          reg
        }
        // Primitive geometry — synchronous
        _ ->
          case
            parse_geometry(geo_spec),
            parse_color(color),
            parse_color(emissive)
          {
            Ok(geometry), Ok(color_int), Ok(emissive_int) -> {
              let material =
                create_material_by_type(
                  material_type,
                  color_int,
                  metalness,
                  roughness,
                  opacity,
                  emissive_int,
                  emissive_intensity,
                  displacement_scale,
                  displacement_bias,
                  shininess,
                  alpha_test,
                  transparent,
                )
              savoiardi.update_material_wireframe(material, wireframe)
              savoiardi.update_material_side(
                material,
                parse_material_side(side),
              )
              let mesh = savoiardi.create_mesh(geometry, material)
              // If parent is a LOD object, register this mesh as a LOD level
              maybe_add_lod_level(reg, parent_id, mesh, distance)
              // Register FIRST so set_transform/set_visible can find the object
              let reg =
                registry.register_and_add_object(
                  reg,
                  id,
                  mesh,
                  parent_id,
                  registry.MeshObject,
                )
              registry.set_transform(reg, id, transform)
              registry.set_visible(reg, id, visible)
              registry.set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
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
            _, _, _ -> reg
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
      let camera = case camera_type {
        "orthographic" ->
          savoiardi.create_orthographic_camera(
            -10.0,
            10.0,
            10.0,
            -10.0,
            near,
            far,
          )
        // Default to perspective
        _ ->
          savoiardi.create_perspective_camera(
            fov,
            registry.get_renderer_aspect_ratio(reg),
            near,
            far,
          )
      }
      // Register FIRST so set_transform can find the object
      let reg = registry.register_camera(reg, id, camera, parent_id, active)
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
      case parse_color(color) {
        Ok(color_int) -> {
          let light = case light_type {
            "directional" -> {
              let light =
                savoiardi.create_directional_light(color_int, intensity)
              case cast_shadow {
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
                savoiardi.create_point_light(color_int, intensity, 0.0)
              case cast_shadow {
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
              savoiardi.create_ambient_light(color_int, intensity)
              |> savoiardi.light_to_object3d
            }
          }
          // Register FIRST so set_transform can find the object
          let reg =
            registry.register_and_add_object(
              reg,
              id,
              light,
              parent_id,
              registry.LightObject,
            )
          registry.set_transform(reg, id, transform)
          reg
        }
        Error(Nil) -> reg
      }
    }

    scene_patch.CreateGroup(id:, parent_id:, transform:, visible:) -> {
      let group = savoiardi.create_group()
      // Register FIRST so set_transform/set_visible can find the object
      let reg =
        registry.register_and_add_object(
          reg,
          id,
          group,
          parent_id,
          registry.GroupObject,
        )
      registry.set_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
      reg
    }

    scene_patch.CreateAudio(
      id:,
      src:,
      volume:,
      loop: audio_loop,
      playing:,
      playback_rate:,
      detune:,
    ) -> {
      let #(reg, listener) = get_or_create_listener(reg)
      let audio = savoiardi.create_audio(listener)
      savoiardi.set_audio_volume(audio, volume)
      savoiardi.set_audio_loop(audio, audio_loop)
      savoiardi.set_audio_playback_rate(audio, playback_rate)
      savoiardi.set_audio_detune(audio, detune)
      load_and_play(audio, src, playing)
      let reg = registry.register_audio(reg, reg.scene_id, id, audio)
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
      let #(reg, listener) = get_or_create_listener(reg)
      let pos_audio = savoiardi.create_positional_audio(listener)
      savoiardi.set_positional_audio_volume(pos_audio, volume)
      savoiardi.set_positional_audio_loop(pos_audio, audio_loop)
      savoiardi.set_positional_audio_playback_rate(pos_audio, playback_rate)
      savoiardi.set_positional_audio_detune(pos_audio, detune)
      savoiardi.set_ref_distance(pos_audio, ref_distance)
      savoiardi.set_max_distance(pos_audio, max_distance)
      savoiardi.set_rolloff_factor(pos_audio, rolloff_factor)
      load_and_play_positional(pos_audio, src, playing)
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
      case parse_color(color) {
        Ok(color_int) -> {
          let object = case debug_type {
            "axes" -> savoiardi.create_axes_helper(size)
            "grid" -> savoiardi.create_grid_helper(size, divisions, color_int)
            // Default to axes
            _ -> savoiardi.create_axes_helper(size)
          }
          // Register FIRST so set_transform can find the object
          let reg =
            registry.register_and_add_object(
              reg,
              id,
              object,
              parent_id,
              registry.DebugObject,
            )
          registry.set_transform(reg, id, transform)
          reg
        }
        Error(Nil) -> reg
      }
    }

    // -- Updates ----------------------------------------------------------------
    scene_patch.UpdateTransform(id:, transform:) -> {
      registry.set_transform(reg, id, transform)
      reg
    }

    scene_patch.UpdateMeshGeometry(id:, geometry: geo_spec) -> {
      case registry.get_object(reg, id) {
        Ok(object) -> {
          savoiardi.get_object_geometry(object)
          |> savoiardi.dispose_geometry
          case parse_geometry(geo_spec) {
            Ok(geometry) -> savoiardi.set_object_geometry(object, geometry)
            Error(Nil) -> Nil
          }
        }
        Error(Nil) -> Nil
      }
      reg
    }

    scene_patch.UpdateMeshSrc(id:, src:) -> {
      update_model(on_async, id, src)
      reg
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
      case registry.get_object(reg, id) {
        Ok(object) ->
          case parse_color(color), parse_color(emissive) {
            Ok(color_int), Ok(emissive_int) -> {
              let new_material =
                create_material_by_type(
                  material_type,
                  color_int,
                  metalness,
                  roughness,
                  opacity,
                  emissive_int,
                  emissive_intensity,
                  displacement_scale,
                  displacement_bias,
                  shininess,
                  alpha_test,
                  transparent,
                )
              savoiardi.update_material_wireframe(new_material, wireframe)
              savoiardi.update_material_side(
                new_material,
                parse_material_side(side),
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
            }
            _, _ -> Nil
          }
        Error(Nil) -> Nil
      }
      reg
    }

    scene_patch.UpdateMeshShadow(id:, cast_shadow:, receive_shadow:) -> {
      registry.set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
      reg
    }

    scene_patch.UpdateMeshVisibility(id:, visible:) -> {
      registry.set_visible(reg, id, visible)
      reg
    }

    scene_patch.UpdateCameraProps(id:, fov:, near:, far:, ..) -> {
      case registry.get_camera(reg, id) {
        Ok(camera) -> {
          savoiardi.set_perspective_camera_params(
            camera,
            fov,
            registry.get_renderer_aspect_ratio(reg),
            near,
            far,
          )
          savoiardi.update_camera_projection_matrix(camera)
        }
        Error(Nil) -> Nil
      }
      reg
    }

    scene_patch.UpdateCameraActive(id:, active:) -> {
      let reg = registry.set_camera_active(reg, id, active)
      // Sync active camera to render loop
      sync_active_camera(reg, loop)
      reg
    }

    scene_patch.UpdateLightProps(id:, color:, intensity:, ..) -> {
      case registry.get_light(reg, id) {
        option.Some(light) ->
          case parse_color(color) {
            Ok(color_int) -> {
              savoiardi.update_light_color(light, color_int)
              savoiardi.update_light_intensity(light, intensity)
            }
            Error(Nil) -> Nil
          }
        option.None -> Nil
      }
      reg
    }

    scene_patch.UpdateAudioProps(
      id:,
      volume:,
      loop: audio_loop,
      playing:,
      playback_rate:,
      detune:,
      ..,
    ) -> {
      case registry.get_audio(reg, id) {
        Ok(audio) -> {
          savoiardi.set_audio_volume(audio, volume)
          savoiardi.set_audio_loop(audio, audio_loop)
          savoiardi.set_audio_playback_rate(audio, playback_rate)
          savoiardi.set_audio_detune(audio, detune)
          update_audio_playback(audio, playing)
        }
        Error(Nil) -> Nil
      }
      reg
    }

    scene_patch.UpdatePositionalAudio(
      id:,
      volume:,
      loop: audio_loop,
      playing:,
      playback_rate:,
      detune:,
      transform:,
      ref_distance:,
      max_distance:,
      rolloff_factor:,
      ..,
    ) -> {
      registry.set_transform(reg, id, transform)
      case registry.get_positional_audio(reg, id) {
        Ok(pos_audio) -> {
          savoiardi.set_positional_audio_volume(pos_audio, volume)
          savoiardi.set_positional_audio_loop(pos_audio, audio_loop)
          savoiardi.set_positional_audio_playback_rate(pos_audio, playback_rate)
          savoiardi.set_positional_audio_detune(pos_audio, detune)
          savoiardi.set_ref_distance(pos_audio, ref_distance)
          savoiardi.set_max_distance(pos_audio, max_distance)
          savoiardi.set_rolloff_factor(pos_audio, rolloff_factor)
          update_positional_audio_playback(pos_audio, playing)
        }
        Error(Nil) -> Nil
      }
      reg
    }

    scene_patch.UpdateGroupVisibility(id:, visible:) -> {
      registry.set_visible(reg, id, visible)
      reg
    }

    // -- LOD creation -----------------------------------------------------------
    scene_patch.CreateLod(id:, parent_id:, transform:) -> {
      let lod = savoiardi.create_lod()
      let object = savoiardi.lod_to_object3d(lod)
      // Register FIRST so set_transform can find the object
      let reg =
        registry.register_and_add_object(
          reg,
          id,
          object,
          parent_id,
          registry.LodObject,
        )
      registry.set_transform(reg, id, transform)
      reg
    }

    // -- InstancedMesh creation -------------------------------------------------
    scene_patch.CreateInstancedMesh(
      id:,
      parent_id:,
      geometry: geo_spec,
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
      case parse_geometry(geo_spec), parse_color(color) {
        Ok(geometry), Ok(color_int) -> {
          let is_transparent = transparent || opacity <. 1.0
          let material =
            create_material_by_type(
              material_type,
              color_int,
              metalness,
              roughness,
              opacity,
              0,
              0.0,
              1.0,
              0.0,
              30.0,
              0.0,
              is_transparent,
            )
          savoiardi.update_material_wireframe(material, wireframe)
          let count = parse_instance_count(instances)
          // Create with generous capacity so count changes don't require recreation.
          let capacity = int.max(count * 4, 64)
          let instanced =
            savoiardi.create_instanced_mesh(geometry, material, capacity)
          // Set the actual rendered count (capacity > count).
          registry.set_instanced_mesh_count(instanced, count)
          let transforms = parse_instance_transforms(instances)
          savoiardi.update_instanced_mesh_transforms(instanced, transforms)
          let object = savoiardi.instanced_mesh_to_object3d(instanced)
          // Register FIRST so set_transform/set_visible/set_mesh_shadow can find it
          let reg =
            registry.register_and_add_object(
              reg,
              id,
              object,
              parent_id,
              registry.InstancedMeshObject,
            )
          registry.set_transform(reg, id, transform)
          registry.set_visible(reg, id, visible)
          registry.set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
          reg
        }
        _, _ -> reg
      }
    }

    scene_patch.UpdateInstancedMeshInstances(id:, instances:) -> {
      case registry.get_instanced_mesh(reg, id) {
        Ok(instanced) -> {
          let count = parse_instance_count(instances)
          let transforms = parse_instance_transforms(instances)
          savoiardi.update_instanced_mesh_transforms(instanced, transforms)
          // Update the rendered count — Three.js only renders .count instances
          registry.set_instanced_mesh_count(instanced, count)
        }
        Error(Nil) -> Nil
      }
      reg
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
      case registry.get_object(reg, id) {
        Ok(object) ->
          case parse_color(color) {
            Ok(color_int) -> {
              let is_transparent = transparent || opacity <. 1.0
              let new_material =
                create_material_by_type(
                  material_type,
                  color_int,
                  metalness,
                  roughness,
                  opacity,
                  0,
                  0.0,
                  1.0,
                  0.0,
                  30.0,
                  0.0,
                  is_transparent,
                )
              savoiardi.update_material_wireframe(new_material, wireframe)
              savoiardi.get_object_material(object)
              |> savoiardi.dispose_material
              savoiardi.set_object_material(object, new_material)
            }
            Error(Nil) -> Nil
          }
        Error(Nil) -> Nil
      }
      reg
    }

    scene_patch.UpdateInstancedMeshVisibility(id:, visible:) -> {
      registry.set_visible(reg, id, visible)
      reg
    }

    scene_patch.UpdateInstancedMeshShadow(id:, cast_shadow:, receive_shadow:) -> {
      registry.set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
      reg
    }

    // -- Structure --------------------------------------------------------------
    scene_patch.Remove(id:) -> {
      let reg = registry.remove_object(reg, id)
      // If a camera was removed, sync the active camera
      sync_active_camera(reg, loop)
      reg
    }

    scene_patch.Reparent(id:, new_parent_id:) -> {
      let reg = registry.reparent_object(reg, id, new_parent_id)
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

fn parse_geometry(spec: String) -> Result(savoiardi.Geometry, Nil) {
  case string.split(spec, ":") {
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
fn get_or_create_listener(
  reg: Registry,
) -> #(Registry, savoiardi.AudioListener) {
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
fn load_and_play(audio: savoiardi.Audio, src: String, should_play: Bool) -> Nil {
  case src {
    "" -> promise.resolve(Error(Nil))
    _ -> {
      use result <- promise.map(savoiardi.load_audio(src))
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
  }
  Nil
}

/// Load an audio buffer and play a PositionalAudio when ready.
fn load_and_play_positional(
  audio: savoiardi.PositionalAudio,
  src: String,
  should_play: Bool,
) -> Nil {
  case src {
    "" -> promise.resolve(Error(Nil))
    _ -> {
      use result <- promise.map(savoiardi.load_audio(src))
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
  }
  Nil
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
  let extension = get_file_extension(src)
  case extension {
    "gltf" | "glb" -> {
      let _ = {
        use result <- promise.map(savoiardi.load_gltf(src))
        case result {
          Ok(data) -> {
            let object = savoiardi.get_gltf_scene(data)
            on_async(fn(reg) {
              let reg =
                registry.register_and_add_object(
                  reg,
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
            on_async(fn(reg) {
              let reg =
                registry.register_and_add_object(
                  reg,
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
            on_async(fn(reg) {
              let reg =
                registry.register_and_add_object(
                  reg,
                  id,
                  data,
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
            on_async(fn(reg) {
              let reg =
                registry.register_and_add_object(
                  reg,
                  id,
                  mesh,
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
          Error(_) -> registry.dispatch_mesh_event(id, "tiramisu:model-error")
        }
      }
      Nil
    }
    _ -> Nil
  }
}

/// Replace an existing object's model when the src attribute changes.
/// Fire-and-forget — dispatches a RegistryTransform via on_async when the
/// new model loads.
fn update_model(
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
            on_async(fn(reg) {
              let reg = registry.replace_object_model(reg, id, object)
              reg
            })
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
            on_async(fn(reg) {
              let reg = registry.replace_object_model(reg, id, object)
              reg
            })
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
            on_async(fn(reg) {
              let reg = registry.replace_object_model(reg, id, data)
              reg
            })
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
fn create_material_by_type(
  material_type: String,
  color_int: Int,
  metalness: Float,
  roughness: Float,
  opacity: Float,
  emissive_int: Int,
  emissive_intensity: Float,
  displacement_scale: Float,
  displacement_bias: Float,
  shininess: Float,
  alpha_test: Float,
  transparent: Bool,
) -> savoiardi.Material {
  let is_transparent = transparent || opacity <. 1.0
  case material_type {
    "basic" ->
      savoiardi.create_basic_material(
        color_int,
        is_transparent,
        opacity,
        option.None,
        savoiardi.FrontSide,
        alpha_test,
        True,
      )
    "phong" ->
      savoiardi.create_phong_material(
        color_int,
        shininess,
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity,
        alpha_test,
      )
    "lambert" ->
      savoiardi.create_lambert_material(
        color_int,
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity,
        alpha_test,
        savoiardi.FrontSide,
      )
    "toon" ->
      savoiardi.create_toon_material(
        color_int,
        option.None,
        option.None,
        option.None,
        is_transparent,
        opacity,
        alpha_test,
      )
    // Default to standard (PBR)
    _ ->
      savoiardi.create_standard_material(
        color_int,
        metalness,
        roughness,
        is_transparent,
        opacity,
        option.None,
        option.None,
        option.None,
        option.None,
        displacement_scale,
        displacement_bias,
        option.None,
        option.None,
        emissive_int,
        emissive_intensity,
        alpha_test,
      )
  }
}

/// Load texture maps from URLs and apply them to a material asynchronously.
fn load_material_textures(
  material: savoiardi.Material,
  color_map: String,
  normal_map: String,
  ao_map: String,
  roughness_map: String,
  metalness_map: String,
  displacement_map: String,
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
  url: String,
  property_name: String,
) -> promise.Promise(Result(Nil, Nil)) {
  case url {
    "" -> promise.resolve(Error(Nil))
    _ -> {
      use result <- promise.map(savoiardi.load_texture(url))
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

/// If the parent of this mesh is a LOD object, register the mesh as a LOD level
/// at the given distance instead of relying on the default parent-child add.
fn maybe_add_lod_level(
  reg: Registry,
  parent_id: String,
  mesh: savoiardi.Object3D,
  distance: Float,
) -> Nil {
  case registry.get_lod(reg, parent_id) {
    Ok(lod) -> savoiardi.add_lod_level(lod:, object: mesh, distance:)
    Error(Nil) -> Nil
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
