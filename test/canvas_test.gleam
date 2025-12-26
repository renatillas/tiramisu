import gleam/list
import gleam/option
import paint as p
import tiramisu/scene
import tiramisu/transform
import vec/vec3

/// Test that adding a canvas generates an AddNode patch
pub fn diff_add_canvas_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev = option.None
  let next =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.AddNode(id: "canvas1", node: _, parent_id: option.None)) =
    list.first(patches)
}

/// Test that removing a canvas generates a RemoveNode patch
pub fn diff_remove_canvas_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))
  let next = option.None

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.RemoveNode(id: "canvas1")) = list.first(patches)
}

/// Test that identical canvas nodes generate no patches (encoding optimization working)
pub fn diff_canvas_no_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))
  let next =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Error(Nil) = list.first(patches)
}

/// Test that changing picture content generates UpdateCanvas patch
pub fn diff_canvas_picture_change_test() {
  let picture1 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))
  let picture2 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(0, 255, 0))

  let prev =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture1,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))
  let next =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture2,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.UpdateCanvas(id: "canvas1", ..)) = list.first(patches)
}

/// Test that changing texture dimensions generates UpdateCanvas patch
pub fn diff_canvas_dimensions_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))
  let next =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 512,
      texture_height: 128,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.UpdateCanvas(id: "canvas1", ..)) = list.first(patches)
}

/// Test that changing canvas size generates UpdateCanvas patch
pub fn diff_canvas_size_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))
  let next =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 4.0,
      height: 1.0,
      transform: transform.identity,
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.UpdateCanvas(id: "canvas1", ..)) = list.first(patches)
}

/// Test that changing transform generates UpdateCanvas patch
pub fn diff_canvas_transform_change_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    ))
  let next =
    option.Some(scene.canvas(
      id: "canvas1",
      picture: picture,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.at(vec3.Vec3(5.0, 10.0, 0.0)),
    ))

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.UpdateCanvas(id: "canvas1", ..)) = list.first(patches)
}

/// Test encoding optimization: Creating the same picture twice should generate identical encoded strings
pub fn canvas_encoding_consistency_test() {
  let picture1 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))
  let picture2 = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let canvas1 =
    scene.canvas(
      id: "canvas1",
      picture: picture1,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    )

  let canvas2 =
    scene.canvas(
      id: "canvas1",
      picture: picture2,
      texture_width: 256,
      texture_height: 64,
      width: 2.0,
      height: 0.5,
      transform: transform.identity,
    )

  // If encoding is consistent, diff should produce no patches
  let #(patches, _) =
    scene.diff(option.Some(canvas1), option.Some(canvas2), option.None)

  let assert Error(Nil) = list.first(patches)
}

/// Test that multiple canvas updates work correctly
pub fn diff_multiple_canvas_test() {
  let picture = p.rectangle(256.0, 64.0) |> p.fill(p.colour_rgb(255, 0, 0))

  let prev =
    option.Some(
      scene.empty(id: "root", transform: transform.identity, children: [
        scene.canvas(
          id: "canvas1",
          picture: picture,
          texture_width: 256,
          texture_height: 64,
          width: 2.0,
          height: 0.5,
          transform: transform.identity,
        ),
      ]),
    )
  let next =
    option.Some(
      scene.empty(id: "root", transform: transform.identity, children: [
        scene.canvas(
          id: "canvas1",
          picture: picture,
          texture_width: 256,
          texture_height: 64,
          width: 2.0,
          height: 0.5,
          transform: transform.identity,
        ),
        scene.canvas(
          id: "canvas2",
          picture: picture,
          texture_width: 256,
          texture_height: 64,
          width: 2.0,
          height: 0.5,
          transform: transform.at(vec3.Vec3(5.0, 0.0, 0.0)),
        ),
      ]),
    )

  let #(patches, _) = scene.diff(prev, next, option.None)

  let assert Ok(scene.AddNode(id: "canvas2", ..)) = list.first(patches)
}
