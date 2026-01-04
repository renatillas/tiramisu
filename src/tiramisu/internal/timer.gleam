// Timer utilities - Simple wrappers around Plinth's timer functions
//
// This module provides a simplified API for working with timers in Tiramisu.
// For now, it's a thin wrapper. Future versions may add automatic pause/resume
// on page visibility changes.

import plinth/javascript/global

// Re-export Plinth's timer types and functions
pub type TimerId =
  global.TimerID

/// Delay execution by a specified duration
pub fn delay(milliseconds: Int, callback: fn() -> Nil) -> Nil {
  global.set_timeout(milliseconds, callback)
  Nil
}

/// Create a recurring interval (returns interval ID)
pub fn interval(milliseconds: Int, callback: fn() -> Nil) -> TimerId {
  global.set_interval(milliseconds, callback)
}

/// Cancel a recurring interval by its ID
pub fn cancel_interval(id: TimerId) -> Nil {
  global.clear_interval(id)
}

/// Set a timeout and return its ID for cancellation
pub fn set_timeout(milliseconds: Int, callback: fn() -> Nil) -> TimerId {
  global.set_timeout(milliseconds, callback)
}

/// Cancel a timeout by its ID
pub fn cancel_timeout(id: TimerId) -> Nil {
  global.clear_timeout(id)
}
