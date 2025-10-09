/// Board operations for Tetris
import gleam/int
import gleam/list
import gleam/set
import tetris/piece.{type Piece, Piece}
import tetris/position.{type Position, Position}

/// The game board - tracks locked blocks
pub type Board {
  Board(locked_blocks: set.Set(Position), width: Int, height: Int)
}

/// Create a new empty board
pub fn new(width: Int, height: Int) -> Board {
  Board(locked_blocks: set.new(), width: width, height: height)
}

/// Standard Tetris board
pub fn standard() -> Board {
  new(10, 20)
}

/// Check if a position is within board bounds
pub fn is_in_bounds(board: Board, pos: Position) -> Bool {
  pos.x >= 0 && pos.x < board.width && pos.y >= 0 && pos.y < board.height
}

/// Check if a position is occupied by a locked block
pub fn is_occupied(board: Board, pos: Position) -> Bool {
  set.contains(board.locked_blocks, pos)
}

/// Check if a position is valid (in bounds and not occupied)
pub fn is_valid_position(board: Board, pos: Position) -> Bool {
  is_in_bounds(board, pos) && !is_occupied(board, pos)
}

/// Check if all positions are valid
pub fn are_positions_valid(board: Board, positions: List(Position)) -> Bool {
  list.all(positions, fn(pos) { is_valid_position(board, pos) })
}

/// Check if a piece would be in a valid position
pub fn is_piece_valid(board: Board, piece: Piece) -> Bool {
  let positions = piece.get_piece_positions(piece)
  are_positions_valid(board, positions)
}

/// Lock a single position onto the board (mainly for testing)
pub fn lock_position(board: Board, pos: Position) -> Board {
  Board(..board, locked_blocks: set.insert(board.locked_blocks, pos))
}

/// Lock a piece onto the board
pub fn lock_piece(board: Board, piece: Piece) -> Board {
  let positions = piece.get_piece_positions(piece)
  let new_locked =
    list.fold(positions, board.locked_blocks, fn(blocks, pos) {
      set.insert(blocks, pos)
    })

  Board(..board, locked_blocks: new_locked)
}

/// Check if a row is complete
pub fn is_row_complete(board: Board, row: Int) -> Bool {
  list.range(0, board.width - 1)
  |> list.all(fn(x) { is_occupied(board, Position(x, row)) })
}

/// Get all complete rows
pub fn get_complete_rows(board: Board) -> List(Int) {
  list.range(0, board.height - 1)
  |> list.filter(fn(row) { is_row_complete(board, row) })
}

/// Remove a row and shift everything above it down
fn remove_row(board: Board, row_to_remove: Int) -> Board {
  let new_locked =
    board.locked_blocks
    |> set.to_list
    // Remove blocks in the completed row
    |> list.filter(fn(pos) { pos.y != row_to_remove })
    // Shift blocks above the row down by one
    |> list.map(fn(pos) {
      case pos.y > row_to_remove {
        True -> Position(x: pos.x, y: pos.y - 1)
        False -> pos
      }
    })
    |> set.from_list

  Board(..board, locked_blocks: new_locked)
}

/// Clear all complete lines and return the new board and number of lines cleared
pub fn clear_lines(board: Board) -> #(Board, Int) {
  let complete_rows = get_complete_rows(board)
  let num_cleared = list.length(complete_rows)

  // Sort rows in descending order so we remove from top to bottom
  // This prevents row index shifting issues when removing multiple lines
  let sorted_rows = list.sort(complete_rows, fn(a, b) { int.compare(b, a) })

  let new_board =
    list.fold(sorted_rows, board, fn(current_board, row) {
      remove_row(current_board, row)
    })

  #(new_board, num_cleared)
}

/// Check if the board has reached game over (blocks in top row)
pub fn is_game_over(board: Board) -> Bool {
  let top_row = board.height - 1
  list.range(0, board.width - 1)
  |> list.any(fn(x) { is_occupied(board, Position(x, top_row)) })
}

/// Try to move left if valid, otherwise return original piece
pub fn try_move_left(board: Board, piece: Piece) -> Piece {
  let new_piece = piece.move_left(piece)
  case is_piece_valid(board, new_piece) {
    True -> new_piece
    False -> piece
  }
}

/// Try to move right if valid, otherwise return original piece
pub fn try_move_right(board: Board, piece: Piece) -> Piece {
  let new_piece = piece.move_right(piece)
  case is_piece_valid(board, new_piece) {
    True -> new_piece
    False -> piece
  }
}

/// Try to move down if valid, otherwise return original piece
pub fn try_move_down(board: Board, piece: Piece) -> Piece {
  let new_piece = piece.move_down(piece)
  case is_piece_valid(board, new_piece) {
    True -> new_piece
    False -> piece
  }
}

/// Try to rotate if valid, otherwise try wall kicks, otherwise return original
pub fn try_rotate(board: Board, piece: Piece) -> Piece {
  let rotated = piece.rotate(piece)

  // Try basic rotation
  case is_piece_valid(board, rotated) {
    True -> rotated
    False -> {
      // Try wall kicks (move left or right to make rotation valid)
      let kick_offsets = [
        Position(1, 0),
        // Try moving right
        Position(-1, 0),
        // Try moving left
        Position(2, 0),
        // Try moving right 2
        Position(-2, 0),
        // Try moving left 2
        Position(0, 1),
        // Try moving up
      ]

      try_wall_kicks(board, rotated, kick_offsets, piece)
    }
  }
}

/// Recursively try wall kicks
fn try_wall_kicks(
  board: Board,
  rotated: Piece,
  offsets: List(Position),
  original: Piece,
) -> Piece {
  case offsets {
    [] -> original
    // No valid wall kick found, return original
    [offset, ..rest] -> {
      let kicked =
        Piece(
          ..rotated,
          position: Position(
            x: rotated.position.x + offset.x,
            y: rotated.position.y + offset.y,
          ),
        )

      case is_piece_valid(board, kicked) {
        True -> kicked
        False -> try_wall_kicks(board, rotated, rest, original)
      }
    }
  }
}

/// Hard drop: move piece down until it can't move anymore
pub fn hard_drop(board: Board, piece: Piece) -> Piece {
  let new_piece = try_move_down(board, piece)

  case new_piece == piece {
    True -> piece
    // Can't move down anymore
    False -> hard_drop(board, new_piece)
    // Keep dropping
  }
}

/// Check if piece can move down
pub fn can_move_down(board: Board, piece: Piece) -> Bool {
  let new_piece = piece.move_down(piece)
  is_piece_valid(board, new_piece)
}
