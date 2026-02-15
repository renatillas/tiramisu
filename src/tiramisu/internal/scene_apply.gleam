//// Pure Gleam patch applier for the scene graph.
////
//// Pattern-matches on ScenePatch variants and calls registry + savoiardi
//// functions. No global state — everything goes through the instance-scoped
//// Registry.

import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import savoiardi
import tiramisu/internal/registry.{type Registry}
import tiramisu/internal/scene_patch.{type ScenePatch}
import tiramisu/transform.{type Transform}
import vec/vec3

/// Apply a list of patches to the Three.js scene graph.
pub fn apply_patches(reg: Registry, patches: List(ScenePatch)) -> Nil {
  list.each(patches, fn(patch) { apply_patch(reg, patch) })
}

fn apply_patch(reg: Registry, patch: ScenePatch) -> Nil {
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
    ) -> {
      case src {
        // External model — load asynchronously
        s if s != "" ->
          load_model(
            reg,
            id,
            parent_id,
            s,
            transform,
            visible,
            cast_shadow,
            receive_shadow,
          )
        // Primitive geometry
        _ -> {
          let geometry = parse_geometry(geo_spec)
          let color_int = parse_color(color)
          let emissive_int = parse_color(emissive)
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
          savoiardi.update_material_side(material, parse_material_side(side))
          let mesh = savoiardi.create_mesh(geometry, material)
          let _ =
            registry.register_and_add_object(
              reg,
              id,
              mesh,
              parent_id,
              registry.MeshObject,
            )
          apply_transform(reg, id, transform)
          registry.set_visible(reg, id, visible)
          set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
          // Load texture maps asynchronously
          load_material_textures(
            material,
            color_map,
            normal_map,
            ao_map,
            roughness_map,
            metalness_map,
            displacement_map,
          )
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
      let _ = registry.register_camera(reg, id, camera, parent_id, active)
      apply_transform(reg, id, transform)
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
      let color_int = parse_color(color)
      let light = case light_type {
        "directional" -> {
          let l = savoiardi.create_directional_light(color_int, intensity)
          case cast_shadow {
            True -> {
              savoiardi.set_light_cast_shadow(l, True)
              savoiardi.configure_shadow(
                l,
                savoiardi.ShadowConfig(
                  resolution: 1024,
                  bias: -0.0001,
                  normal_bias: 0.0,
                ),
              )
              savoiardi.configure_directional_shadow_camera(
                l,
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
          savoiardi.light_to_object3d(l)
        }
        "point" -> {
          let l = savoiardi.create_point_light(color_int, intensity, 0.0)
          case cast_shadow {
            True -> {
              savoiardi.set_light_cast_shadow(l, True)
              savoiardi.configure_shadow(
                l,
                savoiardi.ShadowConfig(
                  resolution: 1024,
                  bias: -0.0001,
                  normal_bias: 0.0,
                ),
              )
            }
            False -> Nil
          }
          savoiardi.light_to_object3d(l)
        }
        // Default to ambient
        _ -> {
          let l = savoiardi.create_ambient_light(color_int, intensity)
          savoiardi.light_to_object3d(l)
        }
      }
      let _ =
        registry.register_and_add_object(
          reg,
          id,
          light,
          parent_id,
          registry.LightObject,
        )
      apply_transform(reg, id, transform)
    }

    scene_patch.CreateGroup(id:, parent_id:, transform:, visible:) -> {
      let group = savoiardi.create_group()
      let _ =
        registry.register_and_add_object(
          reg,
          id,
          group,
          parent_id,
          registry.GroupObject,
        )
      apply_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
    }

    scene_patch.CreateAudio(id:, src:, volume:, loop:, playing:, playback_rate:) -> {
      let listener = get_or_create_listener(reg)
      let audio = savoiardi.create_audio(listener)
      savoiardi.set_audio_volume(audio, volume)
      savoiardi.set_audio_loop(audio, loop)
      savoiardi.set_audio_playback_rate(audio, playback_rate)
      // Register in the scene graph (Audio extends Object3D)
      let _ = registry.register_audio(reg, reg.scene_id, id, audio)
      // Load buffer async, play when ready if requested
      load_and_play(audio, src, playing)
    }

    scene_patch.CreatePositionalAudio(
      id:,
      parent_id:,
      src:,
      volume:,
      loop:,
      playing:,
      playback_rate:,
      transform:,
      ref_distance:,
      max_distance:,
      rolloff_factor:,
    ) -> {
      let listener = get_or_create_listener(reg)
      let pos_audio = savoiardi.create_positional_audio(listener)
      savoiardi.set_positional_audio_volume(pos_audio, volume)
      savoiardi.set_positional_audio_loop(pos_audio, loop)
      savoiardi.set_positional_audio_playback_rate(pos_audio, playback_rate)
      savoiardi.set_ref_distance(pos_audio, ref_distance)
      savoiardi.set_max_distance(pos_audio, max_distance)
      savoiardi.set_rolloff_factor(pos_audio, rolloff_factor)
      // Register in scene graph — children (meshes) will parent to this
      let _ = registry.register_positional_audio(reg, parent_id, id, pos_audio)
      apply_transform(reg, id, transform)
      // Load buffer async, play when ready if requested
      load_and_play_positional(pos_audio, src, playing)
    }

    // -- Updates ----------------------------------------------------------------
    scene_patch.UpdateTransform(id:, transform:) -> {
      apply_transform(reg, id, transform)
    }

    scene_patch.UpdateMeshGeometry(id:, geometry: geo_spec) -> {
      case registry.get_object(reg, id) {
        Ok(object) -> {
          let old_geometry = savoiardi.get_object_geometry(object)
          savoiardi.dispose_geometry(old_geometry)
          let new_geometry = parse_geometry(geo_spec)
          savoiardi.set_object_geometry(object, new_geometry)
        }
        Error(Nil) -> Nil
      }
    }

    scene_patch.UpdateMeshSrc(id:, src:) -> {
      case src {
        "" -> Nil
        _ -> update_model(reg, id, src)
      }
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
        Ok(object) -> {
          let color_int = parse_color(color)
          let emissive_int = parse_color(emissive)
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

          // Load texture maps asynchronously
          load_material_textures(
            new_material,
            color_map,
            normal_map,
            ao_map,
            roughness_map,
            metalness_map,
            displacement_map,
          )
        }
        Error(Nil) -> Nil
      }
    }

    scene_patch.UpdateMeshShadow(id:, cast_shadow:, receive_shadow:) -> {
      set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
    }

    scene_patch.UpdateMeshVisibility(id:, visible:) -> {
      registry.set_visible(reg, id, visible)
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
    }

    scene_patch.UpdateCameraActive(id:, active:) -> {
      registry.set_camera_active(reg, id, active)
    }

    scene_patch.UpdateLightProps(id:, color:, intensity:, ..) -> {
      case registry.get_light(reg, id) {
        option.Some(light) -> {
          let color_int = parse_color(color)
          savoiardi.update_light_color(light, color_int)
          savoiardi.update_light_intensity(light, intensity)
        }
        option.None -> Nil
      }
    }

    scene_patch.UpdateAudioProps(
      id:,
      volume:,
      loop:,
      playing:,
      playback_rate:,
      ..,
    ) -> {
      case registry.get_audio(reg, id) {
        Ok(audio) -> {
          savoiardi.set_audio_volume(audio, volume)
          savoiardi.set_audio_loop(audio, loop)
          savoiardi.set_audio_playback_rate(audio, playback_rate)
          update_audio_playback(audio, playing)
        }
        Error(Nil) -> Nil
      }
    }

    scene_patch.UpdatePositionalAudio(
      id:,
      volume:,
      loop:,
      playing:,
      playback_rate:,
      transform:,
      ref_distance:,
      max_distance:,
      rolloff_factor:,
      ..,
    ) -> {
      apply_transform(reg, id, transform)
      case registry.get_positional_audio(reg, id) {
        Ok(pos_audio) -> {
          savoiardi.set_positional_audio_volume(pos_audio, volume)
          savoiardi.set_positional_audio_loop(pos_audio, loop)
          savoiardi.set_positional_audio_playback_rate(pos_audio, playback_rate)
          savoiardi.set_ref_distance(pos_audio, ref_distance)
          savoiardi.set_max_distance(pos_audio, max_distance)
          savoiardi.set_rolloff_factor(pos_audio, rolloff_factor)
          update_positional_audio_playback(pos_audio, playing)
        }
        Error(Nil) -> Nil
      }
    }

    scene_patch.UpdateGroupVisibility(id:, visible:) -> {
      registry.set_visible(reg, id, visible)
    }

    // -- Structure --------------------------------------------------------------
    scene_patch.Remove(id:) -> {
      let _ = registry.remove_object(reg, id)
      Nil
    }

    scene_patch.Reparent(id:, new_parent_id:) -> {
      let _ = registry.reparent_object(reg, id, new_parent_id)
      Nil
    }
  }
}

// TRANSFORM HELPERS -----------------------------------------------------------

fn apply_transform(reg: Registry, id: String, t: Transform) -> Nil {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  registry.set_position(reg, id, px, py, pz)
  registry.set_quaternion(reg, id, qx, qy, qz, qw)
  registry.set_scale(reg, id, sx, sy, sz)
}

// GEOMETRY PARSING ------------------------------------------------------------

fn parse_geometry(spec: String) -> savoiardi.Geometry {
  case string.split(spec, ":") {
    [type_str, params_str] -> {
      let params =
        string.split(params_str, ",")
        |> list.map(string.trim)
        |> list.filter_map(float.parse)

      case type_str, params {
        "box", [w, h, d] -> savoiardi.create_box_geometry(w, h, d)
        "box", [s] -> savoiardi.create_box_geometry(s, s, s)
        "box", _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)

        "sphere", [r, ws, hs] ->
          savoiardi.create_sphere_geometry(r, float.round(ws), float.round(hs))
        "sphere", [r] -> savoiardi.create_sphere_geometry(r, 32, 16)
        "sphere", _ -> savoiardi.create_sphere_geometry(1.0, 32, 16)

        "plane", [w, h] -> savoiardi.create_plane_geometry(w, h, 1, 1)
        "plane", _ -> savoiardi.create_plane_geometry(1.0, 1.0, 1, 1)

        "cylinder", [rt, rb, h, s] ->
          savoiardi.create_cylinder_geometry(rt, rb, h, float.round(s))
        "cylinder", [r, h] -> savoiardi.create_cylinder_geometry(r, r, h, 32)
        "cylinder", _ -> savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32)

        "cone", [r, h, s] ->
          savoiardi.create_cone_geometry(r, h, float.round(s))
        "cone", [r, h] -> savoiardi.create_cone_geometry(r, h, 32)
        "cone", _ -> savoiardi.create_cone_geometry(1.0, 1.0, 32)

        "torus", [r, t, rs, ts] ->
          savoiardi.create_torus_geometry(
            r,
            t,
            float.round(rs),
            float.round(ts),
          )
        "torus", [r, t] -> savoiardi.create_torus_geometry(r, t, 16, 48)
        "torus", _ -> savoiardi.create_torus_geometry(1.0, 0.4, 16, 48)

        _, _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
      }
    }
    [type_str] -> {
      case type_str {
        "box" -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
        "sphere" -> savoiardi.create_sphere_geometry(1.0, 32, 16)
        "plane" -> savoiardi.create_plane_geometry(1.0, 1.0, 1, 1)
        "cylinder" -> savoiardi.create_cylinder_geometry(1.0, 1.0, 1.0, 32)
        "cone" -> savoiardi.create_cone_geometry(1.0, 1.0, 32)
        "torus" -> savoiardi.create_torus_geometry(1.0, 0.4, 16, 48)
        _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
      }
    }
    _ -> savoiardi.create_box_geometry(1.0, 1.0, 1.0)
  }
}

// COLOR PARSING ---------------------------------------------------------------

fn parse_color(hex: String) -> Int {
  let clean = string.replace(hex, "#", "")
  case int.base_parse(clean, 16) {
    Ok(n) -> n
    Error(_) -> 0xffffff
  }
}

// AUDIO HELPERS ---------------------------------------------------------------

/// Get or create an AudioListener for the given scene.
/// The listener is stored in the registry and attached to the
/// active camera so positional audio panning works correctly.
fn get_or_create_listener(reg: Registry) -> savoiardi.AudioListener {
  case registry.get_audio_listener(reg) {
    Ok(listener) -> listener
    Error(Nil) -> {
      let listener = savoiardi.create_audio_listener()
      let _ = registry.store_audio_listener(reg, listener)
      registry.attach_listener_to_camera(reg, listener)
      listener
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
/// Detects format from URL extension and dispatches events on completion.
fn load_model(
  reg: Registry,
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
      use result <- promise.map(savoiardi.load_gltf(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        registry.dispatch_mesh_event(id, "tiramisu:model-error")
      })

      let object = savoiardi.get_gltf_scene(data)
      let _ =
        registry.register_and_add_object(
          reg,
          id,
          object,
          parent_id,
          registry.MeshObject,
        )

      apply_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
      set_mesh_shadow(reg, id, cast_shadow, receive_shadow)

      registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "fbx" -> {
      use result <- promise.map(savoiardi.load_fbx(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        registry.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let object = savoiardi.get_fbx_scene(data)
      let _ =
        registry.register_and_add_object(
          reg,
          id,
          object,
          parent_id,
          registry.MeshObject,
        )
      apply_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
      set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
      registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "obj" -> {
      use result <- promise.map(savoiardi.load_obj(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        registry.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let _ =
        registry.register_and_add_object(
          reg,
          id,
          data,
          parent_id,
          registry.MeshObject,
        )
      apply_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
      set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
      registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "stl" -> {
      use result <- promise.map(savoiardi.load_stl(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        registry.dispatch_mesh_event(id, "tiramisu:model-error")
      })
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
      let _ =
        registry.register_and_add_object(
          reg,
          id,
          mesh,
          parent_id,
          registry.MeshObject,
        )
      apply_transform(reg, id, transform)
      registry.set_visible(reg, id, visible)
      set_mesh_shadow(reg, id, cast_shadow, receive_shadow)
      registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
      Nil
    }
    _ -> promise.resolve(Error(Nil))
  }
  Nil
}

/// Replace an existing object's model when the src attribute changes.
/// Preserves the current transform and visibility from the old object.
fn update_model(reg: Registry, id: String, src: String) -> Nil {
  let extension = get_file_extension(src)
  case extension {
    "gltf" | "glb" -> {
      use result <- promise.map(savoiardi.load_gltf(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        registry.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let object = savoiardi.get_gltf_scene(data)
      let _ = registry.replace_object_model(reg, id, object)
      registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "fbx" -> {
      use result <- promise.map(savoiardi.load_fbx(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        registry.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let object = savoiardi.get_fbx_scene(data)
      let _ = registry.replace_object_model(reg, id, object)
      registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
      Nil
    }
    "obj" -> {
      use result <- promise.map(savoiardi.load_obj(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        registry.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let _ = registry.replace_object_model(reg, id, data)
      registry.dispatch_mesh_event(id, "tiramisu:model-loaded")
      Nil
    }
    _ -> promise.resolve(Error(Nil))
  }
  Nil
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
) -> Nil {
  load_texture_to_material(material, color_map, "map")
  load_texture_to_material(material, normal_map, "normalMap")
  load_texture_to_material(material, ao_map, "aoMap")
  load_texture_to_material(material, roughness_map, "roughnessMap")
  load_texture_to_material(material, metalness_map, "metalnessMap")
  load_texture_to_material(material, displacement_map, "displacementMap")
}

/// Load a single texture from a URL and set it on a material property.
fn load_texture_to_material(
  material: savoiardi.Material,
  url: String,
  property_name: String,
) -> Nil {
  case url {
    "" -> promise.resolve(Error(Nil))
    _ -> {
      use result <- promise.map(savoiardi.load_texture(url))
      use texture <- result.map(result)
      savoiardi.set_material_texture(material, property_name, texture)
      Nil
    }
  }
  Nil
}

// MATERIAL HELPERS (side conversion) ------------------------------------------

fn parse_material_side(side_str: String) -> savoiardi.MaterialSide {
  case side_str {
    "back" -> savoiardi.BackSide
    "double" -> savoiardi.DoubleSide
    _ -> savoiardi.FrontSide
  }
}

/// Set castShadow/receiveShadow on a mesh via the registry.
fn set_mesh_shadow(
  reg: Registry,
  id: String,
  cast_shadow: Bool,
  receive_shadow: Bool,
) -> Nil {
  case registry.get_object(reg, id) {
    Ok(object) -> savoiardi.enable_shadows(object, cast_shadow, receive_shadow)
    Error(Nil) -> Nil
  }
}
