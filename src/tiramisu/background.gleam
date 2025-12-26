//// Background type for scene rendering.
////
//// Defines what is rendered behind all scene objects. Can be either a solid color,
//// a texture image, or a cube texture (skybox).

import gleam/javascript/promise
import savoiardi
import tiramisu/effect
import tiramisu/texture

pub type Background {
  /// Solid color background (hex color, e.g., 0x111111)
  Color(Int)
  /// 2D texture background loaded from URL or path
  Texture(String)
  /// Equirectangular (360° spherical) texture background
  EquirectangularTexture(String)
  /// Cube texture (skybox) with 6 face images [px, nx, py, ny, pz, nz]
  CubeTexture(List(String))
}

/// Set the scene background dynamically.
///
/// Use `ctx.scene` from the game context to get the scene reference.
///
/// ## Example
///
/// ```gleam
/// fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
///   case msg {
///     LoadSkybox -> {
///       let effect = background.set(
///         ctx.scene,
///         background.CubeTexture(["px.jpg", "nx.jpg", "py.jpg", "ny.jpg", "pz.jpg", "nz.jpg"]),
///         SkyboxLoaded,
///         SkyboxFailed,
///       )
///       #(model, effect, ctx.physics_world)
///     }
///   }
/// }
/// ```
pub fn set(
  game_scene: savoiardi.Scene,
  background: Background,
  on_success: msg,
  on_error: msg,
) -> effect.Effect(msg) {
  effect.from(fn(dispatch) {
    case background {
      Color(color) -> {
        let _ = savoiardi.set_scene_background_color(game_scene, color)
        dispatch(on_success)
      }
      Texture(url) -> {
        texture.load(url)
        |> promise.map(fn(result) {
          case result {
            Ok(texture) -> {
              let _ =
                savoiardi.set_scene_background_texture(game_scene, texture)
              dispatch(on_success)
            }
            Error(_) -> dispatch(on_error)
          }
        })
        Nil
      }
      EquirectangularTexture(url) -> {
        // Load equirectangular texture asynchronously
        texture.load_equirectangular(url)
        |> promise.map(fn(result) {
          case result {
            Ok(texture) -> {
              let _ =
                savoiardi.set_scene_background_texture(game_scene, texture)
              dispatch(on_success)
            }
            Error(_) -> dispatch(on_error)
          }
        })
        Nil
      }
      CubeTexture(urls) -> {
        // Load cube texture asynchronously
        texture.load_cube(urls)
        |> promise.tap(fn(result) {
          case result {
            Ok(cube_texture) -> {
              let _ =
                savoiardi.set_scene_background_cube_texture(
                  game_scene,
                  cube_texture,
                )
              dispatch(on_success)
            }
            Error(_) -> dispatch(on_error)
          }
        })
        Nil
      }
    }
  })
}
