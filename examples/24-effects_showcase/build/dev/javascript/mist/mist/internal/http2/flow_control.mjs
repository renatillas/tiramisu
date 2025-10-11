import * as $int from "../../../../gleam_stdlib/gleam/int.mjs";
import { Ok, Error } from "../../../gleam.mjs";

export function compute_receive_window(receive_window_size, data_size) {
  let new_receive_window_size = receive_window_size - data_size;
  let max_window_increment = $int.bitwise_shift_left(1, 31) - 1;
  let max_window_size = max_window_increment;
  let min_window_size = $int.bitwise_shift_left(1, 30);
  let $ = new_receive_window_size > min_window_size;
  if ($) {
    return [new_receive_window_size, 0];
  } else {
    let updated_receive_window_size = $int.min(
      new_receive_window_size + max_window_increment,
      max_window_size,
    );
    let increment = updated_receive_window_size - new_receive_window_size;
    return [updated_receive_window_size, increment];
  }
}

export function update_send_window(current_send_window, increment) {
  let max_window_size = $int.bitwise_shift_left(1, 31) - 1;
  let update = current_send_window + increment;
  let $ = update > max_window_size;
  if ($) {
    return new Error("Invalid update increment");
  } else {
    return new Ok(update);
  }
}
