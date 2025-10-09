import gleam/list

pub type Rotation {
  R0
  R90
  R180
  R270
}

pub type Position {
  Position(x: Int, y: Int)
}

pub fn rotate_position(pos: Position) -> Position {
  Position(x: pos.y, y: -pos.x)
}

pub fn apply_rotation(
  positions: List(Position),
  rotation: Rotation,
) -> List(Position) {
  case rotation {
    R0 -> positions
    R90 -> list.map(positions, rotate_position)
    R180 -> list.map(positions, fn(p) { rotate_position(rotate_position(p)) })
    R270 ->
      list.map(positions, fn(p) {
        rotate_position(rotate_position(rotate_position(p)))
      })
  }
}
