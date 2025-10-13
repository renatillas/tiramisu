/// Internal ID serialization utilities
///
/// This module provides functions to convert Gleam IDs (which can be any custom type)
/// into stable string representations for use as Map/Dict keys.
///
/// The serialization includes both the type constructor name and the serialized
/// properties to ensure unique keys even for types with no fields.
///
/// ## Examples
///
/// ```gleam
/// type GameId {
///   Player(Int)
///   Enemy(Int)
/// }
///
/// let player_id = Player(1)
/// let enemy_id = Enemy(1)
///
/// // These will serialize to different strings even though they have the same field value
/// id.to_string(player_id)  // "Player:{\"0\":1}"
/// id.to_string(enemy_id)   // "Enemy:{\"0\":1}"
/// ```
/// Convert any Gleam ID to a stable string representation
///
/// This is used internally for object caching, where we need string keys
/// to look up Three.js objects, audio sources, physics bodies, etc.
///
/// The function is polymorphic and works with any Gleam custom type.
pub fn to_string(id: id) -> String {
  serialize_id(id)
}

/// Check if two IDs are equivalent by comparing their serialized forms
///
/// This is useful when you need to compare IDs that might be different
/// instances but represent the same entity.
pub fn equal(a: id, b: id) -> Bool {
  to_string(a) == to_string(b)
}

// FFI function that uses JavaScript reflection to serialize the ID
// This relies on how Gleam compiles custom types to JavaScript classes
@external(javascript, "../../threejs.ffi.mjs", "serializeId")
fn serialize_id(id: id) -> String
