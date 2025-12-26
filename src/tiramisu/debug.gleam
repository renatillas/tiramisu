import savoiardi

/// Render statistics from the WebGL renderer.
///
/// Contains per-frame render metrics useful for optimization.
///
/// ## Fields
/// - `draw_calls`: Number of draw calls in the last frame (lower is better)
/// - `triangles`: Total number of triangles rendered in the last frame
pub type RenderStats {
  RenderStats(draw_calls: Int, triangles: Int)
}

/// Get render statistics from the WebGL renderer.
///
/// Returns the number of draw calls and triangles rendered in the current frame.
/// Use this for optimization - fewer draw calls generally means better performance.
///
/// ## Example
///
/// ```gleam
/// import tiramisu/debug
/// import gleam/io
/// import gleam/int
///
/// pub fn update(model: Model, msg: Msg, ctx: tiramisu.Context) {
///   let stats = debug.get_render_stats(ctx.renderer)
///
///   // Track draw calls for optimization
///   case stats.draw_calls > 100 {
///     True -> io.println("Too many draw calls: " <> int.to_string(stats.draw_calls))
///     False -> Nil
///   }
///
///   // ... rest of update logic
/// }
/// ```
pub fn get_render_stats(renderer: savoiardi.Renderer) -> RenderStats {
  let #(draw_calls, triangles) = savoiardi.get_render_stats(renderer)
  RenderStats(draw_calls: draw_calls, triangles: triangles)
}
