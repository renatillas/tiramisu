import gleam/javascript/promise
import gleam/list
import gleam/option
import gleam_community/maths
import tiramisu
import tiramisu/asset
import tiramisu/camera
import tiramisu/effect.{type Effect}
import tiramisu/geometry
import tiramisu/light
import tiramisu/material
import tiramisu/scene
import tiramisu/transform
import vec/vec3

pub type Model {
  Model(
    rotation: Float,
    light_intensity: Float,
    assets: option.Option(asset.AssetCache),
  )
}

pub type Msg {
  Tick
  AssetsLoaded(asset.BatchLoadResult)
}

pub fn main() -> Nil {
  tiramisu.run(
    background: 0x0a0a0a,
    init: init,
    update: update,
    view: view,
    dimensions: option.None,
  )
}

fn init(_ctx: tiramisu.Context) -> #(Model, Effect(Msg)) {
  // Define all textures to load
  let textures = [
    // Wood floor textures
    asset.TextureAsset("wood-floor/WoodFloor041_1K-JPG_Color.jpg"),
    asset.TextureAsset("wood-floor/WoodFloor041_1K-JPG_NormalGL.jpg"),
    asset.TextureAsset("wood-floor/WoodFloor041_1K-JPG_AmbientOcclusion.jpg"),
    asset.TextureAsset("wood-floor/WoodFloor041_1K-JPG_Roughness.jpg"),
    // Paving stones textures
    asset.TextureAsset("paving-stones/PavingStones142_1K-JPG_Color.jpg"),
    asset.TextureAsset("paving-stones/PavingStones142_1K-JPG_NormalGL.jpg"),
    asset.TextureAsset(
      "paving-stones/PavingStones142_1K-JPG_AmbientOcclusion.jpg",
    ),
    asset.TextureAsset("paving-stones/PavingStones142_1K-JPG_Roughness.jpg"),
    // Onyx textures
    asset.TextureAsset("onyx/Onyx010_1K-JPG_Color.jpg"),
    asset.TextureAsset("onyx/Onyx010_1K-JPG_NormalGL.jpg"),
    asset.TextureAsset("onyx/Onyx010_1K-JPG_Roughness.jpg"),
    // Snow textures
    asset.TextureAsset("snow/Snow008A_1K-JPG_Color.jpg"),
    asset.TextureAsset("snow/Snow008A_1K-JPG_NormalGL.jpg"),
    asset.TextureAsset("snow/Snow008A_1K-JPG_AmbientOcclusion.jpg"),
    asset.TextureAsset("snow/Snow008A_1K-JPG_Roughness.jpg"),
  ]

  let load_effect =
    asset.load_batch_simple(textures)
    |> promise.map(AssetsLoaded)
    |> effect.from_promise

  #(
    Model(rotation: 0.0, light_intensity: 1.0, assets: option.None),
    effect.batch([effect.tick(Tick), load_effect]),
  )
}

fn update(
  model: Model,
  msg: Msg,
  ctx: tiramisu.Context,
) -> #(Model, Effect(Msg)) {
  case msg {
    Tick -> {
      let new_rotation = model.rotation +. ctx.delta_time
      #(Model(..model, rotation: new_rotation), effect.tick(Tick))
    }

    AssetsLoaded(result) -> {
      // Store the loaded assets in the model
      #(Model(..model, assets: option.Some(result.cache)), effect.tick(Tick))
    }
  }
}

fn view(model: Model) -> List(scene.Node) {
  let assert Ok(camera) =
    camera.perspective(field_of_view: 75.0, near: 0.1, far: 1000.0)

  let camera = camera
  let camera =
    scene.Camera(
      id: "main_camera",
      camera:,
      transform: transform.at(position: vec3.Vec3(0.0, 2.0, 10.0)),
      active: True,
      look_at: option.None,
      viewport: option.None,
    )
  let lights = [
    scene.Light(
      id: "ambient",
      light: {
        let assert Ok(light) = light.ambient(color: 0x404040, intensity: 0.3)
        light
      },
      transform: transform.identity,
    ),
    scene.Light(
      id: "directional",
      light: {
        let assert Ok(light) =
          light.directional(color: 0xffffff, intensity: 2.0)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(
          10.0 *. maths.cos(model.rotation),
          10.0,
          10.0 *. maths.sin(model.rotation),
        ),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Light(
      id: "point",
      light: {
        let assert Ok(light) =
          light.point(color: 0xff6b6b, intensity: 1.0, distance: 50.0)
        light
      },
      transform: transform.Transform(
        position: vec3.Vec3(-5.0, 3.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
    scene.Light(
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
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 10.0, 0.0),
        rotation: vec3.Vec3(0.0, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
    ),
  ]

  let assert Ok(box_geom) =
    geometry.sphere(radius: 1.0, width_segments: 100, height_segments: 100)

  // Get textures from cache if loaded
  let wood_color =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "wood-floor/WoodFloor041_1K-JPG_Color.jpg")
      |> option.from_result
    })
  let wood_normal =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "wood-floor/WoodFloor041_1K-JPG_NormalGL.jpg")
      |> option.from_result
    })
  let wood_ao =
    option.then(model.assets, fn(cache) {
      asset.get_texture(
        cache,
        "wood-floor/WoodFloor041_1K-JPG_AmbientOcclusion.jpg",
      )
      |> option.from_result
    })
  let wood_roughness =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "wood-floor/WoodFloor041_1K-JPG_Roughness.jpg")
      |> option.from_result
    })

  let paving_color =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "paving-stones/PavingStones142_1K-JPG_Color.jpg")
      |> option.from_result
    })
  let paving_normal =
    option.then(model.assets, fn(cache) {
      asset.get_texture(
        cache,
        "paving-stones/PavingStones142_1K-JPG_NormalGL.jpg",
      )
      |> option.from_result
    })
  let paving_ao =
    option.then(model.assets, fn(cache) {
      asset.get_texture(
        cache,
        "paving-stones/PavingStones142_1K-JPG_AmbientOcclusion.jpg",
      )
      |> option.from_result
    })
  let paving_roughness =
    option.then(model.assets, fn(cache) {
      asset.get_texture(
        cache,
        "paving-stones/PavingStones142_1K-JPG_Roughness.jpg",
      )
      |> option.from_result
    })

  let onyx_color =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "onyx/Onyx010_1K-JPG_Color.jpg")
      |> option.from_result
    })
  let onyx_normal =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "onyx/Onyx010_1K-JPG_NormalGL.jpg")
      |> option.from_result
    })

  let snow_color =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "snow/Snow008A_1K-JPG_Color.jpg")
      |> option.from_result
    })
  let snow_normal =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "snow/Snow008A_1K-JPG_NormalGL.jpg")
      |> option.from_result
    })
  let snow_ao =
    option.then(model.assets, fn(cache) {
      asset.get_texture(cache, "snow/Snow008A_1K-JPG_AmbientOcclusion.jpg")
      |> option.from_result
    })

  // Create materials with textures
  let assert Ok(basic_mat) =
    material.basic(
      color: 0xff6b6b,
      transparent: False,
      opacity: 1.0,
      map: option.None,
      normal_map: option.None,
    )
  let assert Ok(standard_mat) =
    material.standard(
      color: 0xffffff,
      metalness: 0.0,
      roughness: 1.0,
      map: wood_color,
      normal_map: wood_normal,
      ambient_oclusion_map: wood_ao,
      roughness_map: wood_roughness,
      metalness_map: option.None,
    )
  let assert Ok(phong_mat) =
    material.phong(
      color: 0xffffff,
      shininess: 100.0,
      map: onyx_color,
      normal_map: onyx_normal,
      ambient_oclusion_map: option.None,
    )
  let assert Ok(lambert_mat) =
    material.lambert(
      color: 0xffffff,
      map: snow_color,
      normal_map: snow_normal,
      ambient_oclusion_map: snow_ao,
    )
  let assert Ok(toon_mat) =
    material.toon(
      color: 0xf38181,
      map: option.None,
      normal_map: option.None,
      ambient_oclusion_map: option.None,
    )

  let materials = [
    scene.Mesh(
      id: "basic",
      geometry: box_geom,
      material: basic_mat,
      transform: transform.Transform(
        position: vec3.Vec3(-6.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "standard",
      geometry: box_geom,
      material: standard_mat,
      transform: transform.Transform(
        position: vec3.Vec3(-3.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "phong",
      geometry: box_geom,
      material: phong_mat,
      transform: transform.Transform(
        position: vec3.Vec3(0.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "lambert",
      geometry: box_geom,
      material: lambert_mat,
      transform: transform.Transform(
        position: vec3.Vec3(3.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
    scene.Mesh(
      id: "toon",
      geometry: box_geom,
      material: toon_mat,
      transform: transform.Transform(
        position: vec3.Vec3(6.0, 2.0, 0.0),
        rotation: vec3.Vec3(0.0, model.rotation, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  let assert Ok(plane_geom) = geometry.plane(width: 20.0, height: 20.0)
  let assert Ok(ground_mat) =
    material.standard(
      color: 0xffffff,
      metalness: 0.0,
      roughness: 1.0,
      map: paving_color,
      normal_map: paving_normal,
      ambient_oclusion_map: paving_ao,
      roughness_map: paving_roughness,
      metalness_map: option.None,
    )

  let ground = [
    scene.Mesh(
      id: "ground",
      geometry: plane_geom,
      material: ground_mat,
      transform: transform.Transform(
        position: vec3.Vec3(0.0, -2.0, 0.0),
        rotation: vec3.Vec3(-1.5708, 0.0, 0.0),
        scale: vec3.Vec3(1.0, 1.0, 1.0),
      ),
      physics: option.None,
    ),
  ]

  list.flatten([[camera], lights, materials, ground])
}
