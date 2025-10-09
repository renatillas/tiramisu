/// UI overlay for Tetris game using Lustre
import gleam/int
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import tetris/game
import tetris/piece

/// Render the UI overlay
pub fn view(state: game.GameState, on_restart: msg) -> Element(msg) {
  html.div([attribute.class("flex flex-col p-5 gap-5")], [
    // Top bar with score and stats
    html.div(
      [
        attribute.class(
          "flex gap-8 bg-black/70 px-5 py-4 rounded-xl backdrop-blur-sm",
        ),
      ],
      [
        stat_display("Score", int.to_string(state.score)),
        stat_display("Level", int.to_string(state.level)),
        stat_display("Lines", int.to_string(state.lines_cleared)),
      ],
    ),
    // Next piece preview
    html.div(
      [attribute.class("bg-black/70 p-4 rounded-xl w-40 backdrop-blur-sm")],
      [
        html.h3([attribute.class("text-xs uppercase opacity-70 mb-3")], [
          element.text("Next"),
        ]),
        next_piece_display(state.next_piece),
      ],
    ),
    // Controls info
    html.div(
      [attribute.class("bg-black/70 p-4 rounded-xl w-52 backdrop-blur-sm")],
      [
        html.h3([attribute.class("text-xs uppercase opacity-70 mb-3")], [
          element.text("Controls"),
        ]),
        control_item("←/→ or A/D", "Move"),
        control_item("↑ or W", "Rotate"),
        control_item("↓ or S", "Soft Drop"),
        control_item("Space", "Hard Drop"),
      ],
    ),
    // Game over overlay
    case state.game_over {
      True -> game_over_overlay(state.score, on_restart)
      False -> element.none()
    },
  ])
}

fn stat_display(label: String, value: String) -> Element(a) {
  html.div([attribute.class("flex flex-col items-center")], [
    html.div([attribute.class("text-xs uppercase opacity-70 mb-1")], [
      element.text(label),
    ]),
    html.div([attribute.class("text-2xl font-bold")], [element.text(value)]),
  ])
}

fn next_piece_display(piece_type: piece.Shape) -> Element(a) {
  let color = piece.piece_color(piece_type)
  let color_hex = "#" <> int.to_base16(color)

  html.div(
    [
      attribute.class(
        "w-full h-16 rounded-lg flex items-center justify-center text-3xl font-bold",
      ),
      attribute.style("background-color", color_hex),
    ],
    [element.text(piece_type_name(piece_type))],
  )
}

fn piece_type_name(piece_type: piece.Shape) -> String {
  case piece_type {
    piece.I -> "I"
    piece.O -> "O"
    piece.T -> "T"
    piece.S -> "S"
    piece.Z -> "Z"
    piece.J -> "J"
    piece.L -> "L"
  }
}

fn control_item(keys: String, action: String) -> Element(a) {
  html.div([attribute.class("flex justify-between my-2 text-xs")], [
    html.span([attribute.class("font-bold opacity-90")], [element.text(keys)]),
    html.span([attribute.class("opacity-70")], [element.text(action)]),
  ])
}

fn game_over_overlay(score: Int, on_restart: msg) -> Element(msg) {
  html.div(
    [
      attribute.class(
        "fixed top-0 left-0 w-full h-full bg-black/90 flex items-center justify-center pointer-events-auto",
      ),
    ],
    [
      html.div(
        [
          attribute.class(
            "text-center p-10 bg-white/10 rounded-3xl backdrop-blur-sm",
          ),
        ],
        [
          html.h1([attribute.class("text-5xl mb-5 text-red-500")], [
            element.text("Game Over"),
          ]),
          html.p([attribute.class("text-2xl my-3")], [
            element.text("Final Score: " <> int.to_string(score)),
          ]),
          html.button(
            [
              attribute.class(
                "mt-8 px-8 py-3 bg-blue-600 hover:bg-blue-700 rounded-xl text-xl font-bold transition-colors cursor-pointer",
              ),
              attribute.type_("button"),
              event.on_click(on_restart),
            ],
            [element.text("Play Again")],
          ),
        ],
      ),
    ],
  )
}
