/// Piece operations for Tetris
import gleam/list
import tetris/position.{
  type Position, type Rotation, Position, R0, R180, R270, R90,
}

/// The seven tetromino piece types
pub type Shape {
  I
  O
  T
  S
  Z
  J
  L
}

/// A tetromino piece with its type, position, and rotation
pub type Piece {
  Piece(piece_type: Shape, position: Position, rotation: Rotation)
}

/// Create a new piece at the spawn position (center-top of board)
pub fn spawn(piece_type: Shape, board_width: Int) -> Piece {
  Piece(
    piece_type: piece_type,
    position: Position(x: board_width / 2, y: 17),
    rotation: R0,
  )
}

/// Move a piece left
pub fn move_left(piece: Piece) -> Piece {
  Piece(..piece, position: Position(..piece.position, x: piece.position.x - 1))
}

/// Move a piece right
pub fn move_right(piece: Piece) -> Piece {
  Piece(..piece, position: Position(..piece.position, x: piece.position.x + 1))
}

/// Move a piece down
pub fn move_down(piece: Piece) -> Piece {
  Piece(..piece, position: Position(..piece.position, y: piece.position.y - 1))
}

/// Rotate a piece clockwise
pub fn rotate(piece: Piece) -> Piece {
  Piece(..piece, rotation: next_rotation(piece.rotation))
}

pub fn piece_blocks(piece_type: Shape) -> List(Position) {
  case piece_type {
    I -> [Position(0, 0), Position(0, 1), Position(0, 2), Position(0, -1)]
    O -> [Position(0, 0), Position(1, 0), Position(0, 1), Position(1, 1)]
    T -> [Position(0, 0), Position(-1, 0), Position(1, 0), Position(0, 1)]
    S -> [Position(0, 0), Position(-1, 0), Position(0, 1), Position(1, 1)]
    Z -> [Position(0, 0), Position(1, 0), Position(0, 1), Position(-1, 1)]
    J -> [Position(0, 0), Position(-1, 0), Position(0, 1), Position(0, 2)]
    L -> [Position(0, 0), Position(1, 0), Position(0, 1), Position(0, 2)]
  }
}

/// Get the next rotation state
pub fn next_rotation(rotation: Rotation) -> Rotation {
  case rotation {
    R0 -> R90
    R90 -> R180
    R180 -> R270
    R270 -> R0
  }
}

pub fn get_piece_positions(piece: Piece) -> List(Position) {
  let local_blocks = piece_blocks(piece.piece_type)
  let rotated_blocks = position.apply_rotation(local_blocks, piece.rotation)

  list.map(rotated_blocks, fn(local_pos) {
    Position(
      x: piece.position.x + local_pos.x,
      y: piece.position.y + local_pos.y,
    )
  })
}

/// Get the color for a piece type (hex color)
pub fn piece_color(piece_type: Shape) -> Int {
  case piece_type {
    I -> 0x00f0f0
    // Cyan
    O -> 0xf0f000
    // Yellow
    T -> 0xa000f0
    // Purple
    S -> 0x00f000
    // Green
    Z -> 0xf00000
    // Red
    J -> 0x0000f0
    // Blue
    L -> 0xf0a000
    // Orange
  }
}
