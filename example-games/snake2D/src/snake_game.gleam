//// Main function and entry point

import gleam/option
import model
import tiramisu
import tiramisu/background
import view

pub fn main() -> Nil {
  tiramisu.run(
    dimensions: option.None,
    background: background.Color(0x162b1e),
    init: model.init,
    update: model.update,
    view: view.view,
  )
}
