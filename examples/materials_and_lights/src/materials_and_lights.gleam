import gleam/dict
import gleam/float
import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam/time/duration
import gleam_community/maths
import tiramisu
import tiramisu/asset
import tiramisu/camera
import tiramisu/effect
import tiramisu/geometry
import tiramisu/input
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/texture
import tiramisu/transform
import vec/vec3

pub type LoadedTextures {
  LoadedTextures(
    wood_color: texture.Texture,
    wood_normal: texture.Texture,
    wood_ao: texture.Texture,
    wood_roughness: texture.Texture,
    wood_displacement: texture.Texture,
    paving_color: texture.Texture,
    paving_normal: texture.Texture,
    paving_ao: texture.Texture,
    paving_roughness: texture.Texture,
    onyx_color: texture.Texture,
    onyx_normal: texture.Texture,
    snow_color: texture.Texture,
    snow_normal: texture.Texture,
    snow_ao: texture.Texture,
  )
}

pub type Model {
  Model(
    rotation: Float,
    light_intensity: Float,
    textures: option.Option(LoadedTextures),
    camera_position: vec3.Vec3(Float),
    camera_rotation: vec3.Vec3(Float),
    pointer_locked: Bool,
  )
}

pub type Msg {
  Tick
  TexturesLoaded(dict.Dict(String, Result(texture.Texture, Nil)))
  PointerLocked
  PointerLockFailed
}

pub fn main() -> Nil {
  let assert Ok(Nil) =
    tiramisu.run(
      selector: "body",
      init: init,
      update: update,
      view: view,
      dimensions: option.None,
    )
  Nil
}

fn init(
  _ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  // Load all textures in parallel
  let keys = [
    "wood_color",
    "wood_normal",
    "wood_ao",
    "wood_roughness",
    "wood_displacement",
    "paving_color",
    "paving_normal",
    "paving_ao",
    "paving_roughness",
    "onyx_color",
    "onyx_normal",
    "snow_color",
    "snow_normal",
    "snow_ao",
  ]

  let texture_promises = [
    asset.load_texture("wood-floor/WoodFloor041_1K-JPG_Color.jpg"),
    asset.load_texture("wood-floor/WoodFloor041_1K-JPG_NormalGL.jpg"),
    asset.load_texture("wood-floor/WoodFloor041_1K-JPG_AmbientOcclusion.jpg"),
    asset.load_texture("wood-floor/WoodFloor041_1K-JPG_Roughness.jpg"),
    asset.load_texture("wood-floor/WoodFloor041_1K-JPG_Displacement.jpg"),
    asset.load_texture("paving-stones/PavingStones142_1K-JPG_Color.jpg"),
    asset.load_texture("paving-stones/PavingStones142_1K-JPG_NormalGL.jpg"),
    asset.load_texture(
      "paving-stones/PavingStones142_1K-JPG_AmbientOcclusion.jpg",
    ),
    asset.load_texture("paving-stones/PavingStones142_1K-JPG_Roughness.jpg"),
    asset.load_texture("onyx/Onyx010_1K-JPG_Color.jpg"),
    asset.load_texture("onyx/Onyx010_1K-JPG_NormalGL.jpg"),
    asset.load_texture("snow/Snow008A_1K-JPG_Color.jpg"),
    asset.load_texture("snow/Snow008A_1K-JPG_NormalGL.jpg"),
    asset.load_texture("snow/Snow008A_1K-JPG_AmbientOcclusion.jpg"),
  ]

  let load_effect =
    texture_promises
    |> promise.await_list
    |> promise.map(fn(results) {
      list.zip(keys, results)
      |> dict.from_list
    })
    |> promise.map(TexturesLoaded)
    |> effect.from_promise

  #(
    Model(
      rotation: 0.0,
      light_intensity: 1.0,
      textures: option.None,
      camera_position: vec3.Vec3(0.0, 2.0, 10.0),
      camera_rotation: vec3.Vec3(0.0, 0.0, 0.0),
      pointer_locked: False,
    ),
    effect.batch([effect.tick(Tick), load_effect]),
    option.None,
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, effect.Effect(Msg), option.Option(_)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. duration.to_seconds(ctx.delta_time)

      // Request pointer lock on click or 'C' key (when not already locked)
      let should_request_lock = case model.pointer_locked {
        False ->
          input.is_left_button_just_pressed(ctx.input)
          || input.is_key_just_pressed(ctx.input, input.KeyC)
        True -> False
      }

      let pointer_lock_effect = case should_request_lock {
        True ->
          effect.request_pointer_lock(
            on_success: PointerLocked,
            on_error: PointerLockFailed,
          )
        False -> effect.none()
      }

      // Exit pointer lock on Escape key
      let #(should_exit_pointer_lock, exit_lock_effect) = case
        input.is_key_just_pressed(ctx.input, input.Escape),
        model.pointer_locked
      {
        True, True -> #(True, effect.exit_pointer_lock())
        _, _ -> #(False, effect.none())
      }

      // Camera movement speed
      let move_speed = 5.0 *. duration.to_seconds(ctx.delta_time)
      let mouse_sensitivity = 0.003

      // Handle rotation input with mouse delta first
      let vec3.Vec3(cam_pitch, cam_yaw, cam_roll) = model.camera_rotation
      let #(mouse_dx, mouse_dy) = input.mouse_delta(ctx.input)

      let #(cam_yaw, cam_pitch) = case model.pointer_locked {
        True -> {
          // Use mouse delta for rotation when pointer is locked
          let new_yaw = cam_yaw -. mouse_dx *. mouse_sensitivity
          let new_pitch = cam_pitch -. mouse_dy *. mouse_sensitivity
          #(new_yaw, new_pitch)
        }
        False ->
          case input.is_right_button_pressed(ctx.input) {
            True -> {
              // Use mouse delta when right mouse button is held (not locked)
              let new_yaw = cam_yaw -. mouse_dx *. mouse_sensitivity
              let new_pitch = cam_pitch -. mouse_dy *. mouse_sensitivity
              #(new_yaw, new_pitch)
            }
            False -> #(cam_yaw, cam_pitch)
          }
      }

      // Clamp pitch to avoid flipping
      let cam_pitch = float.max(-1.5, float.min(1.5, cam_pitch))

      // Calculate forward/right vectors based on NEW camera rotation
      // For movement, we only use yaw (horizontal rotation) to avoid flying up/down
      let forward_x = maths.sin(cam_yaw)
      let forward_z = maths.cos(cam_yaw)
      let right_x = maths.cos(cam_yaw)
      let right_z = 0.0 -. maths.sin(cam_yaw)

      // Handle movement input (WASD)
      let vec3.Vec3(cam_x, cam_y, cam_z) = model.camera_position

      let cam_x = case input.is_key_pressed(ctx.input, input.KeyW) {
        True -> cam_x +. forward_x *. move_speed
        False -> cam_x
      }
      let cam_z = case input.is_key_pressed(ctx.input, input.KeyW) {
        True -> cam_z +. forward_z *. move_speed
        False -> cam_z
      }

      let cam_x = case input.is_key_pressed(ctx.input, input.KeyS) {
        True -> cam_x -. forward_x *. move_speed
        False -> cam_x
      }
      let cam_z = case input.is_key_pressed(ctx.input, input.KeyS) {
        True -> cam_z -. forward_z *. move_speed
        False -> cam_z
      }

      let cam_x = case input.is_key_pressed(ctx.input, input.KeyD) {
        True -> cam_x -. right_x *. move_speed
        False -> cam_x
      }
      let cam_z = case input.is_key_pressed(ctx.input, input.KeyD) {
        True -> cam_z -. right_z *. move_speed
        False -> cam_z
      }

      let cam_x = case input.is_key_pressed(ctx.input, input.KeyA) {
        True -> cam_x +. right_x *. move_speed
        False -> cam_x
      }
      let cam_z = case input.is_key_pressed(ctx.input, input.KeyA) {
        True -> cam_z +. right_z *. move_speed
        False -> cam_z
      }

      // Vertical movement (Space/Shift)
      let cam_y = case input.is_key_pressed(ctx.input, input.Space) {
        True -> cam_y +. move_speed
        False -> cam_y
      }
      let cam_y = case input.is_key_pressed(ctx.input, input.ShiftLeft) {
        True -> cam_y -. move_speed
        False -> cam_y
      }

      // Update pointer_locked state if we're exiting
      let pointer_locked = case should_exit_pointer_lock {
        True -> False
        False -> model.pointer_locked
      }

      #(
        Model(
          ..model,
          rotation: new_rotation,
          camera_position: vec3.Vec3(cam_x, cam_y, cam_z),
          camera_rotation: vec3.Vec3(cam_pitch, cam_yaw, cam_roll),
          pointer_locked: pointer_locked,
        ),
        effect.batch([effect.tick(Tick), pointer_lock_effect, exit_lock_effect]),
        option.None,
      )
    }

    TexturesLoaded(texture_dict) -> {
      // Extract all textures from the dict
      let textures = case
        dict.get(texture_dict, "wood_color"),
        dict.get(texture_dict, "wood_normal"),
        dict.get(texture_dict, "wood_ao"),
        dict.get(texture_dict, "wood_roughness"),
        dict.get(texture_dict, "wood_displacement"),
        dict.get(texture_dict, "paving_color"),
        dict.get(texture_dict, "paving_normal"),
        dict.get(texture_dict, "paving_ao"),
        dict.get(texture_dict, "paving_roughness"),
        dict.get(texture_dict, "onyx_color"),
        dict.get(texture_dict, "onyx_normal"),
        dict.get(texture_dict, "snow_color"),
        dict.get(texture_dict, "snow_normal"),
        dict.get(texture_dict, "snow_ao")
      {
        Ok(Ok(wood_color)),
          Ok(Ok(wood_normal)),
          Ok(Ok(wood_ao)),
          Ok(Ok(wood_roughness)),
          Ok(Ok(wood_displacement)),
          Ok(Ok(paving_color)),
          Ok(Ok(paving_normal)),
          Ok(Ok(paving_ao)),
          Ok(Ok(paving_roughness)),
          Ok(Ok(onyx_color)),
          Ok(Ok(onyx_normal)),
          Ok(Ok(snow_color)),
          Ok(Ok(snow_normal)),
          Ok(Ok(snow_ao))
        ->
          option.Some(LoadedTextures(
            wood_color:,
            wood_normal:,
            wood_ao:,
            wood_roughness:,
            wood_displacement:,
            paving_color:,
            paving_normal:,
            paving_ao:,
            paving_roughness:,
            onyx_color:,
            onyx_normal:,
            snow_color:,
            snow_normal:,
            snow_ao:,
          ))
        _, _, _, _, _, _, _, _, _, _, _, _, _, _ -> option.None
      }

      #(Model(..model, textures:), effect.none(), option.None)
    }

    PointerLocked -> {
      // Pointer lock was successfully activated
      #(Model(..model, pointer_locked: True), effect.none(), option.None)
    }

    PointerLockFailed -> {
      // Pointer lock failed (user might have denied it)
      #(Model(..model, pointer_locked: False), effect.none(), option.None)
    }
  }
}

fn view(model: Model, _) -> scene.Node {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  // Calculate look-at target for FPS camera
  // This ensures pitch and yaw work correctly in world space
  let vec3.Vec3(cam_pitch, cam_yaw, _) = model.camera_rotation
  let vec3.Vec3(cam_x, cam_y, cam_z) = model.camera_position

  // Calculate forward direction from yaw and pitch
  let forward_x = maths.sin(cam_yaw) *. maths.cos(cam_pitch)
  let forward_y = maths.sin(cam_pitch)
  let forward_z = maths.cos(cam_yaw) *. maths.cos(cam_pitch)

  // Look-at target is position + forward direction
  let look_at_target =
    vec3.Vec3(cam_x +. forward_x, cam_y +. forward_y, cam_z +. forward_z)

  let camera = camera
  let camera =
    scene.camera(
      id: "main_camera",
      camera:,
      transform: transform.at(position: model.camera_position),
      active: True,
      look_at: option.Some(look_at_target),
      viewport: option.None,
      postprocessing: option.None,
    )
  let lights =
    scene.empty(id: "lights", transform: transform.identity, children: [
      scene.light(
        id: "ambient",
        light: {
          let assert Ok(light) = light.ambient(color: 0x404040, intensity: 0.3)
          light
        },
        transform: transform.identity,
      ),
      scene.light(
        id: "directional",
        light: {
          let assert Ok(light) =
            light.directional(color: 0xffffff, intensity: 2.0)
          light
        },
        transform: transform.at(position: vec3.Vec3(
          10.0 *. maths.cos(model.rotation),
          10.0,
          10.0 *. maths.sin(model.rotation),
        )),
      ),
      scene.light(
        id: "point",
        light: {
          let assert Ok(light) =
            light.point(color: 0xff6b6b, intensity: 1.0, distance: 50.0)
          light
        },
        transform: transform.at(position: vec3.Vec3(-5.0, 3.0, 0.0)),
      ),
      scene.light(
        id: "hemisphere",
        light: {
          let assert Ok(light) =
            light.hemisphere(
              sky_color: 0xffffff,
              ground_color: 0xff0000,
              intensity: 1.0,
            )
          light
        },
        transform: transform.at(position: vec3.Vec3(0.0, 10.0, 0.0)),
      ),
    ])

  let assert Ok(box_geom) =
    geometry.sphere(radius: 1.0, width_segments: 100, height_segments: 100)

  // Get textures from model if loaded
  let wood_color = option.map(model.textures, fn(t) { t.wood_color })
  let wood_normal = option.map(model.textures, fn(t) { t.wood_normal })
  let wood_ambient_oclusion = option.map(model.textures, fn(t) { t.wood_ao })
  let wood_roughness = option.map(model.textures, fn(t) { t.wood_roughness })
  let wood_displacement =
    option.map(model.textures, fn(t) { t.wood_displacement })

  let paving_color = option.map(model.textures, fn(t) { t.paving_color })
  let paving_normal = option.map(model.textures, fn(t) { t.paving_normal })
  let paving_ao = option.map(model.textures, fn(t) { t.paving_ao })
  let paving_roughness =
    option.map(model.textures, fn(t) { t.paving_roughness })

  let onyx_color = option.map(model.textures, fn(t) { t.onyx_color })
  let onyx_normal = option.map(model.textures, fn(t) { t.onyx_normal })

  let snow_color = option.map(model.textures, fn(t) { t.snow_color })
  let snow_normal = option.map(model.textures, fn(t) { t.snow_normal })
  let snow_ao = option.map(model.textures, fn(t) { t.snow_ao })

  // Create materials with textures
  let assert Ok(basic_mat) =
    material.basic(
      color: 0xff6b6b,
      transparent: False,
      opacity: 1.0,
      map: option.None,
    )

  let standard_material = case
    wood_color,
    wood_normal,
    wood_ambient_oclusion,
    wood_roughness,
    wood_displacement
  {
    option.Some(color),
      option.Some(normal),
      option.Some(ambient_oclusion),
      option.Some(roughness),
      option.Some(displacement)
    -> {
      let assert Ok(standard_mat) =
        material.new()
        |> material.with_color(0xffffff)
        |> material.with_metalness(0.0)
        |> material.with_roughness(1.0)
        |> material.with_color_map(color)
        |> material.with_normal_map(normal)
        |> material.with_ambient_oclusion_map(ambient_oclusion)
        |> material.with_displacement_map(displacement)
        |> material.with_displacement_scale(0.3)
        |> material.with_displacement_bias(-0.15)
        |> material.with_roughness_map(roughness)
        |> material.build()
      standard_mat
    }
    _, _, _, _, _ -> {
      let assert Ok(standard_mat) =
        material.new()
        |> material.build()
      standard_mat
    }
  }

  let assert Ok(phong_mat) =
    material.phong(
      color: 0xffffff,
      shininess: 100.0,
      map: onyx_color,
      normal_map: onyx_normal,
      ambient_oclusion_map: option.None,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )
  let assert Ok(lambert_mat) =
    material.lambert(
      color: 0xffffff,
      map: snow_color,
      normal_map: snow_normal,
      ambient_oclusion_map: snow_ao,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )
  let assert Ok(toon_mat) =
    material.toon(
      color: 0xf38181,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
      transparent: False,
      opacity: 1.0,
      alpha_test: 0.0,
    )

  let spheres =
    scene.empty(id: "spheres", transform: transform.identity, children: [
      scene.mesh(
        id: "basic",
        geometry: box_geom,
        material: basic_mat,
        transform: transform.at(position: vec3.Vec3(-6.0, 2.0, 0.0))
          |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
        physics: option.None,
      ),
      scene.mesh(
        id: "standard",
        geometry: box_geom,
        material: standard_material,
        transform: transform.at(position: vec3.Vec3(-3.0, 2.0, 0.0))
          |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
        physics: option.None,
      ),
      scene.mesh(
        id: "phong",
        geometry: box_geom,
        material: phong_mat,
        transform: transform.at(position: vec3.Vec3(0.0, 2.0, 0.0))
          |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
        physics: option.None,
      ),
      scene.mesh(
        id: "lambert",
        geometry: box_geom,
        material: lambert_mat,
        transform: transform.at(position: vec3.Vec3(3.0, 2.0, 0.0))
          |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
        physics: option.None,
      ),
      scene.mesh(
        id: "toon",
        geometry: box_geom,
        material: toon_mat,
        transform: transform.at(position: vec3.Vec3(6.0, 2.0, 0.0))
          |> transform.with_euler_rotation(vec3.Vec3(0.0, model.rotation, 0.0)),
        physics: option.None,
      ),
    ])

  let assert Ok(plane_geom) = geometry.plane(width: 20.0, height: 20.0)
  let ground_material = case
    paving_color,
    paving_normal,
    paving_ao,
    paving_roughness
  {
    option.Some(color),
      option.Some(normal),
      option.Some(ao),
      option.Some(roughness)
    -> {
      let assert Ok(ground_mat) =
        material.new()
        |> material.with_color(0xffffff)
        |> material.with_metalness(0.0)
        |> material.with_roughness(1.0)
        |> material.with_color_map(color)
        |> material.with_normal_map(normal)
        |> material.with_ambient_oclusion_map(ao)
        |> material.with_roughness_map(roughness)
        |> material.build()
      ground_mat
    }
    _, _, _, _ -> {
      let assert Ok(ground_mat) =
        material.new()
        |> material.build()
      ground_mat
    }
  }

  let ground =
    scene.mesh(
      id: "ground",
      geometry: plane_geom,
      material: ground_material,
      transform: transform.at(position: vec3.Vec3(0.0, -2.0, 0.0))
        |> transform.with_euler_rotation(vec3.Vec3(-1.5708, 0.0, 0.0)),
      physics: option.None,
    )

  scene.empty(id: "scene", transform: transform.identity, children: [
    camera,
    lights,
    camera,
    spheres,
    ground,
  ])
}
