import gleam/option
import paint as p
import tiramisu/scene
import tiramisu/transform
import vec/vec3

type TestId {
  Sprite1
  Sprite2
}

/// Test that adding a sprite generates an AddNode patch
pub fn diff_add_sprite_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = []
  let next = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.AddNode(id: Sprite1, node: _, parent_id: option.None)] -> Nil
    _ -> panic as "Expected AddNode patch for sprite"
  }
}

/// Test that removing a sprite generates a RemoveNode patch
pub fn diff_remove_sprite_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]
  let next = []

  let patches = scene.diff(prev, next)

  case patches {
    [scene.RemoveNode(id: Sprite1)] -> Nil
    _ -> panic as "Expected RemoveNode patch for sprite"
  }
}

/// Test that identical sprites generate no patches (encoding optimization working)
pub fn diff_sprite_no_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]
  let next = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [] -> Nil
    _ ->
      panic as "Expected no patches for identical sprites (encoding optimization should prevent re-rendering)"
  }
}

/// Test that changing picture content generates UpdateSprite patch
pub fn diff_sprite_picture_change_test() {
  let picture1 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))
  let picture2 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(0, 255, 0))

  let prev = [
    scene.sprite(
      id: Sprite1,
      picture: picture1,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]
  let next = [
    scene.sprite(
      id: Sprite1,
      picture: picture2,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.UpdateSprite(id: Sprite1, ..)] -> Nil
    _ -> panic as "Expected UpdateSprite patch when picture changes"
  }
}

/// Test that changing texture dimensions generates UpdateSprite patch
pub fn diff_sprite_dimensions_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]
  let next = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 512,
      texture_height: 128,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.UpdateSprite(id: Sprite1, ..)] -> Nil
    _ -> panic as "Expected UpdateSprite patch when texture dimensions change"
  }
}

/// Test that changing sprite size generates UpdateSprite patch
pub fn diff_sprite_size_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]
  let next = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 4.0,
      height: 1.0,
      transform: transform.identity,
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.UpdateSprite(id: Sprite1, ..)] -> Nil
    _ -> panic as "Expected UpdateSprite patch when sprite size changes"
  }
}

/// Test that changing transform generates UpdateSprite patch
pub fn diff_sprite_transform_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]
  let next = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.at(vec3.Vec3(5.0, 10.0, 0.0)),
    ),
  ]

  let patches = scene.diff(prev, next)

  case patches {
    [scene.UpdateSprite(id: Sprite1, ..)] -> Nil
    _ -> panic as "Expected UpdateSprite patch when transform changes"
  }
}

/// Test encoding optimization: Creating the same picture twice should generate identical encoded strings
pub fn sprite_encoding_consistency_test() {
  let picture1 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))
  let picture2 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let sprite1 = scene.sprite(
    id: Sprite1,
    picture: picture1,
    texture_width: 256,
    texture_height: 64,
    width: 2.0,
    height: 0.5,
    transform: transform.identity,
  )

  let sprite2 = scene.sprite(
    id: Sprite1,
    picture: picture2,
    texture_width: 256,
    texture_height: 64,
    width: 2.0,
    height: 0.5,
    transform: transform.identity,
  )

  // If encoding is consistent, diff should produce no patches
  let patches = scene.diff([sprite1], [sprite2])

  case patches {
    [] -> Nil
    _ ->
      panic as "Expected no patches - identical pictures should encode to identical strings"
  }
}

/// Test that multiple sprite updates work correctly
pub fn diff_multiple_sprites_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
  ]
  let next = [
    scene.sprite(
      id: Sprite1,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ),
    scene.sprite(
      id: Sprite2,
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.at(vec3.Vec3(5.0, 0.0, 0.0)),
    ),
  ]

  let patches = scene.diff(prev, next)

  // Should only have AddNode for Sprite2, no update for Sprite1
  case patches {
    [scene.AddNode(id: Sprite2, ..)] -> Nil
    _ -> panic as "Expected only AddNode patch for new sprite"
  }
}
