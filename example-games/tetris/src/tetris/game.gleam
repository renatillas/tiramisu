/// Game state management for Tetris
import gleam/int
import tetris/board
import tetris/piece

pub type GameState {
  GameState(
    board: board.Board,
    current_piece: piece.Piece,
    next_piece: piece.Shape,
    score: Int,
    lines_cleared: Int,
    level: Int,
    game_over: Bool,
  )
}

/// Create a new game
pub fn new() -> GameState {
  let board = board.standard()
  let first_piece = random_piece_type()
  let next_piece = random_piece_type()

  GameState(
    board: board,
    current_piece: piece.spawn(first_piece, board.width),
    next_piece: next_piece,
    score: 0,
    lines_cleared: 0,
    level: 1,
    game_over: False,
  )
}

pub fn random_piece_type() -> piece.Shape {
  case int.random(7) {
    0 -> piece.I
    1 -> piece.O
    2 -> piece.T
    3 -> piece.S
    4 -> piece.Z
    5 -> piece.J
    _ -> piece.L
  }
}

/// Move current piece left
pub fn move_left(state: GameState) -> GameState {
  case state.game_over {
    True -> state
    False -> {
      let new_piece = board.try_move_left(state.board, state.current_piece)
      GameState(..state, current_piece: new_piece)
    }
  }
}

/// Move current piece right
pub fn move_right(state: GameState) -> GameState {
  case state.game_over {
    True -> state
    False -> {
      let new_piece = board.try_move_right(state.board, state.current_piece)
      GameState(..state, current_piece: new_piece)
    }
  }
}

/// Rotate current piece
pub fn rotate(state: GameState) -> GameState {
  case state.game_over {
    True -> state
    False -> {
      let new_piece = board.try_rotate(state.board, state.current_piece)
      GameState(..state, current_piece: new_piece)
    }
  }
}

/// Hard drop current piece
pub fn hard_drop(state: GameState) -> GameState {
  case state.game_over {
    True -> state
    False -> {
      let dropped_piece = board.hard_drop(state.board, state.current_piece)
      lock_and_spawn_next(GameState(..state, current_piece: dropped_piece))
    }
  }
}

/// Soft drop (move down one step)
pub fn soft_drop(state: GameState) -> GameState {
  case state.game_over {
    True -> state
    False -> {
      case board.can_move_down(state.board, state.current_piece) {
        True -> {
          let new_piece = board.try_move_down(state.board, state.current_piece)
          GameState(..state, current_piece: new_piece)
        }
        False -> lock_and_spawn_next(state)
      }
    }
  }
}

/// Lock current piece and spawn next piece
fn lock_and_spawn_next(state: GameState) -> GameState {
  // Lock the current piece
  let new_board = board.lock_piece(state.board, state.current_piece)

  // Clear lines
  let #(cleared_board, num_cleared) = board.clear_lines(new_board)

  // Calculate score
  let line_score = case num_cleared {
    0 -> 0
    1 -> 100
    2 -> 300
    3 -> 500
    4 -> 800
    _ -> 800
  }
  let new_score = state.score + line_score * state.level
  let new_lines_cleared = state.lines_cleared + num_cleared
  let new_level = 1 + new_lines_cleared / 10

  // Spawn next piece
  let next_piece_obj = piece.spawn(state.next_piece, cleared_board.width)
  let next_next_piece = random_piece_type()

  // Check game over
  let is_valid = board.is_piece_valid(cleared_board, next_piece_obj)
  let is_board_game_over = board.is_game_over(cleared_board)
  let game_over = !is_valid || is_board_game_over

  GameState(
    board: cleared_board,
    current_piece: next_piece_obj,
    next_piece: next_next_piece,
    score: new_score,
    lines_cleared: new_lines_cleared,
    level: new_level,
    game_over: game_over,
  )
}

/// Update game state (called on timer tick)
pub fn tick(state: GameState) -> GameState {
  soft_drop(state)
}

/// Calculate drop interval in seconds based on level
pub fn drop_interval(level: Int) -> Float {
  let base_interval = 1.0
  let speed_multiplier = int.to_float(level) *. 0.1
  case base_interval -. speed_multiplier {
    x if x <. 0.1 -> 0.1
    x -> x
  }
}
