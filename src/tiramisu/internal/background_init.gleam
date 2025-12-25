// Background initialization - Set scene background during startup
//
// This module handles setting the initial background when the game starts.
// For runtime background changes, use `effect.set_background()`.

import gleam/javascript/promise
import tiramisu/background.{type Background}
import tiramisu/scene
import tiramisu/texture

/// Set the initial background on a Three.js scene
/// This is called during game initialization (tiramisu.run)
pub fn set_initial_background(scene: scene.Scene, bg: Background) -> Nil {
  case bg {
    background.Color(color) -> set_scene_background_color(scene, color)
    background.Texture(url) -> {
      load_texture(url)
      |> promise.tap(fn(tex) { set_scene_background_texture(scene, tex) })
      Nil
    }
    background.EquirectangularTexture(url) -> {
      load_equirectangular_texture(url)
      |> promise.tap(fn(tex) { set_scene_background_texture(scene, tex) })
      Nil
    }
    background.CubeTexture(urls) -> {
      load_cube_texture(urls)
      |> promise.tap(fn(tex) { set_scene_background_cube_texture(scene, tex) })
      Nil
    }
  }
}

// FFI bindings to threejs.ffi.mjs
@external(javascript, "../../threejs.ffi.mjs", "setSceneBackgroundColor")
fn set_scene_background_color(scene: scene.Scene, color: Int) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setSceneBackgroundTexture")
fn set_scene_background_texture(
  scene: scene.Scene,
  texture: texture.Texture,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "setSceneBackgroundCubeTexture")
fn set_scene_background_cube_texture(
  scene: scene.Scene,
  texture: texture.Texture,
) -> Nil

@external(javascript, "../../threejs.ffi.mjs", "loadTexture")
fn load_texture(url: String) -> promise.Promise(texture.Texture)

@external(javascript, "../../threejs.ffi.mjs", "loadEquirectangularTexture")
fn load_equirectangular_texture(url: String) -> promise.Promise(texture.Texture)

@external(javascript, "../../threejs.ffi.mjs", "loadCubeTexture")
fn load_cube_texture(urls: List(String)) -> promise.Promise(texture.Texture)
