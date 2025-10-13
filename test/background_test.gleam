import gleam/list
import gleeunit
import tiramisu/background

pub fn main() {
  gleeunit.main()
}

// Background Constructor Tests

pub fn color_background_test() {
  let background.Color(color) = background.Color(0x111111)
  assert color == 0x111111
}

pub fn texture_background_test() {
  let background.Texture(url) = background.Texture("assets/sky.jpg")
  assert url == "assets/sky.jpg"
}

pub fn cube_texture_background_test() {
  let urls = [
    "assets/skybox/px.jpg",
    "assets/skybox/nx.jpg",
    "assets/skybox/py.jpg",
    "assets/skybox/ny.jpg",
    "assets/skybox/pz.jpg",
    "assets/skybox/nz.jpg",
  ]
  let background.CubeTexture(cube_urls) = background.CubeTexture(urls)

  assert list.length(cube_urls) == 6
  assert list.first(cube_urls) == Ok("assets/skybox/px.jpg")
}
