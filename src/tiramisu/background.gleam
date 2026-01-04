//// Scene background configuration.
////
//// Set the background of your 3D scene to a solid color, 2D texture,
//// equirectangular panorama, or cubemap skybox.
////
//// ## Example
////
//// ```gleam
//// // Set a solid color background
//// background.set(ctx.scene, background.Color(0x1a1a2e), BackgroundSet, BackgroundFailed)
////
//// // Load a skybox
//// background.set(
////   ctx.scene,
////   background.CubeTexture(["px.jpg", "nx.jpg", "py.jpg", "ny.jpg", "pz.jpg", "nz.jpg"]),
////   SkyboxLoaded,
////   SkyboxFailed,
//// )
//// ```
////

import gleam/javascript/promise
import savoiardi

import tiramisu/effect

pub type Background {
  /// Solid color background (hex color, e.g., 0x111111)
  Color(Int)
  /// 2D texture background loaded from URL or path
  Texture(String)
  /// Equirectangular (360° spherical) texture background
  EquirectangularTexture(String)
  /// TODO: This should be a type instead of a list of strings
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
  on_success on_success: msg,
  on_error on_error: msg,
) -> effect.Effect(msg) {
  effect.from(fn(dispatch) {
    case background {
      Color(color) -> {
        let _ = savoiardi.set_scene_background_color(game_scene, color)
        dispatch(on_success)
      }
      Texture(url) -> {
        savoiardi.load_texture(url)
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
        savoiardi.load_equirectangular_texture(url)
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
        savoiardi.load_cube_texture(urls)
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
