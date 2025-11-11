//// Helper functions used by view and by model update for distance calculations

import tiramisu

pub const box_width = 50.0

pub fn left_border(ctx: tiramisu.Context(String)) -> Float {
  0.0 -. half_vert_calc(ctx)
}

pub fn right_border(ctx: tiramisu.Context(String)) -> Float {
  half_vert_calc(ctx)
}

fn half_vert_calc(ctx: tiramisu.Context(String)) -> Float {
  ctx.canvas_width /. 2.0
}

pub fn upper_border(ctx: tiramisu.Context(String)) -> Float {
  half_horz_calc(ctx)
}

pub fn down_border(ctx: tiramisu.Context(String)) -> Float {
  0.0 -. half_horz_calc(ctx)
}

fn half_horz_calc(ctx: tiramisu.Context(String)) -> Float {
  ctx.canvas_height /. 2.0 -. horz_border_dist()
}

pub fn horz_border_dist() -> Float {
  3.0 *. box_width
}
