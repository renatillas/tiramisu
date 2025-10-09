import gleam/list
import gleeunit
import tetris/board
import tetris/piece.{I, O, Piece, T}
import tetris/position.{Position, R0, R90}

pub fn main() {
  gleeunit.main()
}

// Piece tests

pub fn piece_blocks_i_test() {
  let blocks = piece.piece_blocks(I)
  let assert [Position(0, 0), Position(0, 1), Position(0, 2), Position(0, -1)] =
    blocks
}

pub fn piece_blocks_o_test() {
  let blocks = piece.piece_blocks(O)
  let assert [Position(0, 0), Position(1, 0), Position(0, 1), Position(1, 1)] =
    blocks
}

pub fn rotate_position_test() {
  let pos = Position(1, 0)
  let rotated = position.rotate_position(pos)
  let assert Position(0, -1) = rotated

  let pos2 = Position(0, 1)
  let rotated2 = position.rotate_position(pos2)
  let assert Position(1, 0) = rotated2
}

pub fn next_rotation_test() {
  assert piece.next_rotation(R0) == R90
  assert piece.next_rotation(R90) == position.R180
  assert piece.next_rotation(position.R180) == position.R270
  assert piece.next_rotation(position.R270) == R0
}

// Board tests

pub fn new_board_test() {
  let test_board = board.new(10, 20)
  assert test_board.width == 10
  assert test_board.height == 20
}

pub fn is_in_bounds_test() {
  let test_board = board.standard()

  assert board.is_in_bounds(test_board, Position(0, 0)) == True
  assert board.is_in_bounds(test_board, Position(9, 19)) == True
  assert board.is_in_bounds(test_board, Position(-1, 0)) == False
  assert board.is_in_bounds(test_board, Position(10, 0)) == False
  assert board.is_in_bounds(test_board, Position(0, 20)) == False
}

pub fn lock_piece_test() {
  let test_board = board.standard()
  let test_piece = piece.spawn(I, test_board.width)
  let _locked_board = board.lock_piece(test_board, test_piece)

  // Check that piece positions are now occupied
  let positions = piece.get_piece_positions(test_piece)
  assert positions != []
}

pub fn is_row_complete_test() {
  let test_board = board.standard()

  // Lock a position at each x coordinate in row 0
  let positions =
    list.range(0, 9)
    |> list.map(fn(x) { Position(x, 0) })

  let board_with_full_row =
    list.fold(positions, test_board, fn(b, pos) { board.lock_position(b, pos) })

  assert board.is_row_complete(board_with_full_row, 0) == True
  assert board.is_row_complete(board_with_full_row, 1) == False
}

// Piece movement tests

pub fn spawn_piece_test() {
  let test_piece = piece.spawn(T, 10)
  assert test_piece.piece_type == T
  assert test_piece.position == Position(5, 17)
  assert test_piece.rotation == R0
}

pub fn move_left_test() {
  let test_piece = piece.spawn(I, 10)
  let moved = piece.move_left(test_piece)
  assert moved.position.x == test_piece.position.x - 1
}

pub fn move_right_test() {
  let test_piece = piece.spawn(I, 10)
  let moved = piece.move_right(test_piece)
  assert moved.position.x == test_piece.position.x + 1
}

pub fn move_down_test() {
  let test_piece = piece.spawn(I, 10)
  let moved = piece.move_down(test_piece)
  assert moved.position.y == test_piece.position.y - 1
}

pub fn rotate_test() {
  let test_piece = piece.spawn(I, 10)
  let rotated = piece.rotate(test_piece)
  assert rotated.rotation == R90
}

pub fn can_move_down_test() {
  let test_board = board.standard()
  let test_piece = piece.spawn(I, test_board.width)

  // Should be able to move down initially
  assert board.can_move_down(test_board, test_piece) == True

  // Move piece to bottom
  let bottom_piece =
    Piece(..test_piece, position: Position(test_piece.position.x, 0))

  assert board.can_move_down(test_board, bottom_piece) == False
}

pub fn clear_multiple_lines_test() {
  let test_board = board.standard()

  // Fill rows 0 and 1 completely
  let row_0_positions =
    list.range(0, 9)
    |> list.map(fn(x) { Position(x, 0) })

  let row_1_positions =
    list.range(0, 9)
    |> list.map(fn(x) { Position(x, 1) })

  let board_with_two_full_rows =
    list.fold(row_0_positions, test_board, fn(b, pos) {
      board.lock_position(b, pos)
    })
    |> fn(b) {
      list.fold(row_1_positions, b, fn(b, pos) { board.lock_position(b, pos) })
    }

  // Add a block in row 2 to verify it shifts down correctly
  let board_with_block_above =
    board.lock_position(board_with_two_full_rows, Position(5, 2))

  // Clear the lines
  let #(cleared_board, lines_cleared) = board.clear_lines(board_with_block_above)

  // Should have cleared 2 lines
  assert lines_cleared == 2

  // Row 0 and 1 should now be empty
  assert board.is_row_complete(cleared_board, 0) == False
  assert board.is_row_complete(cleared_board, 1) == False

  // The block that was in row 2 should now be in row 0 (shifted down by 2)
  assert board.is_occupied(cleared_board, Position(5, 0)) == True
  assert board.is_occupied(cleared_board, Position(5, 2)) == False
}
