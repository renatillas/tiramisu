//// Pure Gleam patch applier for the scene graph.
////
//// Pattern-matches on ScenePatch variants and calls existing runtime.gleam
//// functions.

import gleam/float
import gleam/int
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import savoiardi
import tiramisu/internal/runtime
import tiramisu/internal/scene_patch.{type ScenePatch}
import tiramisu/transform.{type Transform}
import vec/vec3

/// Apply a list of patches to the Three.js scene graph.
pub fn apply_patches(scene_id: String, patches: List(ScenePatch)) -> Nil {
  list.each(patches, fn(patch) { apply_patch(scene_id, patch) })
}

fn apply_patch(scene_id: String, patch: ScenePatch) -> Nil {
  let scene_ref = runtime.SceneRef(scene_id)

  case patch {
    scene_patch.CreateMesh(
      id:,
      parent_id:,
      geometry: geo_spec,
      src:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe: _wireframe,
      transform:,
      visible:,
    ) -> {
      case src {
        // External model — load asynchronously
        s if s != "" ->
          load_model(scene_ref, id, parent_id, s, transform, visible)
        // Primitive geometry
        _ -> {
          let geometry = parse_geometry(geo_spec)
          let color_int = parse_color(color)
          let material =
            savoiardi.create_standard_material(
              color_int,
              metalness,
              roughness,
              opacity <. 1.0,
              opacity,
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
            )
          let obj_ref =
            runtime.create_mesh(scene_ref, parent_id, id, geometry, material)
          apply_transform_to_ref(obj_ref, transform)
          runtime.set_visible(obj_ref, visible)
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
      let camera_ref = case camera_type {
        "orthographic" ->
          runtime.create_orthographic_camera(
            scene_ref,
            parent_id,
            id,
            -10.0,
            10.0,
            10.0,
            -10.0,
            near,
            far,
            active,
          )
        // Default to perspective
        _ ->
          runtime.create_perspective_camera(
            scene_ref,
            parent_id,
            id,
            fov,
            16.0 /. 9.0,
            near,
            far,
            active,
          )
      }
      let obj_ref = runtime.ObjectRef(camera_ref.id)
      apply_transform_to_ref(obj_ref, transform)
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
      let obj_ref = case light_type {
        "directional" ->
          runtime.create_directional_light(
            scene_ref,
            parent_id,
            id,
            color_int,
            intensity,
            cast_shadow,
          )
        "point" ->
          runtime.create_point_light(
            scene_ref,
            parent_id,
            id,
            color_int,
            intensity,
            0.0,
            cast_shadow,
          )
        // Default to ambient
        _ ->
          runtime.create_ambient_light(
            scene_ref,
            parent_id,
            id,
            color_int,
            intensity,
          )
      }
      apply_transform_to_ref(obj_ref, transform)
    }

    scene_patch.CreateGroup(id:, parent_id:, transform:, visible:) -> {
      let obj_ref = runtime.create_group(scene_ref, parent_id, id)
      apply_transform_to_ref(obj_ref, transform)
      runtime.set_visible(obj_ref, visible)
    }

    scene_patch.CreateAudio(id:, src:, volume:, loop:, playing:, playback_rate:) -> {
      let listener = get_or_create_listener(scene_ref)
      let audio = savoiardi.create_audio(listener)
      savoiardi.set_audio_volume(audio, volume)
      savoiardi.set_audio_loop(audio, loop)
      savoiardi.set_audio_playback_rate(audio, playback_rate)
      // Register in the scene graph (Audio extends Object3D)
      let _ = runtime.add_audio(scene_ref, scene_id, id, audio)
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
      let listener = get_or_create_listener(scene_ref)
      let pos_audio = savoiardi.create_positional_audio(listener)
      savoiardi.set_positional_audio_volume(pos_audio, volume)
      savoiardi.set_positional_audio_loop(pos_audio, loop)
      savoiardi.set_positional_audio_playback_rate(pos_audio, playback_rate)
      savoiardi.set_ref_distance(pos_audio, ref_distance)
      savoiardi.set_max_distance(pos_audio, max_distance)
      savoiardi.set_rolloff_factor(pos_audio, rolloff_factor)
      // Register in scene graph — children (meshes) will parent to this
      let obj_ref =
        runtime.add_positional_audio(scene_ref, parent_id, id, pos_audio)
      apply_transform_to_ref(obj_ref, transform)
      // Load buffer async, play when ready if requested
      load_and_play_positional(pos_audio, src, playing)
    }

    // -- Updates ----------------------------------------------------------------
    scene_patch.UpdateTransform(id:, transform:) -> {
      let ref = runtime.ObjectRef(id:)
      apply_transform_to_ref(ref, transform)
    }

    scene_patch.UpdateMeshGeometry(id:, geometry: geo_spec) -> {
      let ref = runtime.ObjectRef(id:)
      case runtime.get_object(ref) {
        option.Some(object) -> {
          let old_geometry = savoiardi.get_object_geometry(object)
          savoiardi.dispose_geometry(old_geometry)
          let new_geometry = parse_geometry(geo_spec)
          savoiardi.set_object_geometry(object, new_geometry)
        }
        option.None -> Nil
      }
    }

    scene_patch.UpdateMeshSrc(id:, src:) -> {
      case src {
        "" -> Nil
        _ -> {
          let ref = runtime.ObjectRef(id:)
          update_model(ref, id, src)
        }
      }
    }

    scene_patch.UpdateMeshMaterial(
      id:,
      color:,
      metalness:,
      roughness:,
      opacity:,
      wireframe:,
    ) -> {
      let ref = runtime.ObjectRef(id:)
      case runtime.get_object(ref) {
        option.Some(object) -> {
          let material = savoiardi.get_object_material(object)
          let color_int = parse_color(color)
          savoiardi.update_material_color(material, color_int)
          savoiardi.update_material_metalness(material, metalness)
          savoiardi.update_material_roughness(material, roughness)
          savoiardi.update_material_opacity(material, opacity)
          savoiardi.update_material_wireframe(material, wireframe)
        }
        option.None -> Nil
      }
    }

    scene_patch.UpdateMeshVisibility(id:, visible:) -> {
      let ref = runtime.ObjectRef(id:)
      runtime.set_visible(ref, visible)
    }

    scene_patch.UpdateCameraProps(id:, fov:, near:, far:, ..) -> {
      let ref = runtime.CameraRef(id:)
      case runtime.get_camera(ref) {
        option.Some(camera) -> {
          savoiardi.set_perspective_camera_params(
            camera,
            fov,
            16.0 /. 9.0,
            near,
            far,
          )
          savoiardi.update_camera_projection_matrix(camera)
        }
        option.None -> Nil
      }
    }

    scene_patch.UpdateCameraActive(id:, active:) -> {
      let ref = runtime.CameraRef(id:)
      runtime.set_camera_active(ref, active)
    }

    scene_patch.UpdateLightProps(id:, color:, intensity:, ..) -> {
      let ref = runtime.ObjectRef(id:)
      case runtime.get_light(ref) {
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
      let ref = runtime.ObjectRef(id:)
      case runtime.get_audio(ref) {
        option.Some(audio) -> {
          savoiardi.set_audio_volume(audio, volume)
          savoiardi.set_audio_loop(audio, loop)
          savoiardi.set_audio_playback_rate(audio, playback_rate)
          update_audio_playback(audio, playing)
        }
        option.None -> Nil
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
      let ref = runtime.ObjectRef(id:)
      apply_transform_to_ref(ref, transform)
      case runtime.get_positional_audio(ref) {
        option.Some(pos_audio) -> {
          savoiardi.set_positional_audio_volume(pos_audio, volume)
          savoiardi.set_positional_audio_loop(pos_audio, loop)
          savoiardi.set_positional_audio_playback_rate(pos_audio, playback_rate)
          savoiardi.set_ref_distance(pos_audio, ref_distance)
          savoiardi.set_max_distance(pos_audio, max_distance)
          savoiardi.set_rolloff_factor(pos_audio, rolloff_factor)
          update_positional_audio_playback(pos_audio, playing)
        }
        option.None -> Nil
      }
    }

    scene_patch.UpdateGroupVisibility(id:, visible:) -> {
      let ref = runtime.ObjectRef(id:)
      runtime.set_visible(ref, visible)
    }

    // -- Structure --------------------------------------------------------------
    scene_patch.Remove(id:) -> {
      let ref = runtime.ObjectRef(id:)
      runtime.remove_object(ref)
    }

    scene_patch.Reparent(id:, new_parent_id:) -> {
      // Reparenting requires removing from old parent and adding to new.
      // Three.js parent.add() automatically removes from old parent.
      let ref = runtime.ObjectRef(id:)
      runtime.reparent_object(ref, new_parent_id, scene_ref)
    }
  }
}

// TRANSFORM HELPERS -----------------------------------------------------------

fn apply_transform_to_ref(ref: runtime.ObjectRef, t: Transform) -> Nil {
  let vec3.Vec3(px, py, pz) = transform.position(t)
  let #(qx, qy, qz, qw) = transform.to_quaternion_xyzw(t)
  let vec3.Vec3(sx, sy, sz) = transform.scale(t)

  runtime.set_position(ref, px, py, pz)
  runtime.set_quaternion(ref, qx, qy, qz, qw)
  runtime.set_scale(ref, sx, sy, sz)
}

// GEOMETRY PARSING ------------------------------------------------------------
// Extracted from mesh.gleam — same logic.

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
/// The listener is stored in the runtime registry and attached to the
/// active camera so positional audio panning works correctly.
fn get_or_create_listener(
  scene_ref: runtime.SceneRef,
) -> savoiardi.AudioListener {
  case runtime.get_audio_listener(scene_ref) {
    option.Some(listener) -> listener
    option.None -> {
      let listener = savoiardi.create_audio_listener()
      runtime.store_audio_listener(scene_ref, listener)
      runtime.attach_listener_to_camera(scene_ref, listener)
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
  scene_ref: runtime.SceneRef,
  id: String,
  parent_id: String,
  src: String,
  transform: Transform,
  visible: Bool,
) -> Nil {
  let extension = get_file_extension(src)
  case extension {
    "gltf" | "glb" -> {
      use result <- promise.map(savoiardi.load_gltf(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        runtime.dispatch_mesh_event(id, "tiramisu:model-error")
      })

      let object = savoiardi.get_gltf_scene(data)
      let obj_ref =
        runtime.add_object_to_scene(scene_ref, parent_id, id, object)

      apply_transform_to_ref(obj_ref, transform)

      runtime.set_visible(obj_ref, visible)

      runtime.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "fbx" -> {
      use result <- promise.map(savoiardi.load_fbx(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        runtime.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let object = savoiardi.get_fbx_scene(data)
      let obj_ref =
        runtime.add_object_to_scene(scene_ref, parent_id, id, object)
      apply_transform_to_ref(obj_ref, transform)
      runtime.set_visible(obj_ref, visible)
      runtime.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "obj" -> {
      use result <- promise.map(savoiardi.load_obj(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        runtime.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let obj_ref = runtime.add_object_to_scene(scene_ref, parent_id, id, data)
      apply_transform_to_ref(obj_ref, transform)
      runtime.set_visible(obj_ref, visible)
      runtime.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "stl" -> {
      use result <- promise.map(savoiardi.load_stl(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        runtime.dispatch_mesh_event(id, "tiramisu:model-error")
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
        )
      let obj_ref =
        runtime.create_mesh(scene_ref, parent_id, id, centered, material)
      apply_transform_to_ref(obj_ref, transform)
      runtime.set_visible(obj_ref, visible)
      runtime.dispatch_mesh_event(id, "tiramisu:model-loaded")
      Nil
    }
    _ -> promise.resolve(Error(Nil))
  }
  Nil
}

/// Replace an existing object's model when the src attribute changes.
/// Preserves the current transform and visibility from the old object.
fn update_model(ref: runtime.ObjectRef, id: String, src: String) -> Nil {
  let extension = get_file_extension(src)
  case extension {
    "gltf" | "glb" -> {
      use result <- promise.map(savoiardi.load_gltf(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        runtime.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let object = savoiardi.get_gltf_scene(data)
      runtime.replace_object_model(ref, object)
      runtime.dispatch_mesh_event(id, "tiramisu:model-loaded")
    }
    "fbx" -> {
      use result <- promise.map(savoiardi.load_fbx(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        runtime.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      let object = savoiardi.get_fbx_scene(data)
      runtime.replace_object_model(ref, object)
      runtime.dispatch_mesh_event(id, "tiramisu:model-loaded")
      Nil
    }
    "obj" -> {
      use result <- promise.map(savoiardi.load_obj(src))
      use data <- result.map({
        use _ <- result.map_error(result)
        runtime.dispatch_mesh_event(id, "tiramisu:model-error")
      })
      runtime.replace_object_model(ref, data)
      runtime.dispatch_mesh_event(id, "tiramisu:model-loaded")
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
