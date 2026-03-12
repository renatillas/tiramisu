import gleam/bytes_tree
import gleam/erlang/application
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import mist.{type Connection, type ResponseData}
import scene_component
import tiramisu

pub fn main() {
  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(request) {
        [] -> serve_html()
        ["lustre", "runtime.mjs"] -> serve_lustre_runtime()
        ["client", ..segments] -> serve_client_asset(segments)
        ["ws"] -> serve_scene_component(request)
        _ -> not_found()
      }
    }
    |> mist.new
    |> mist.bind("localhost")
    |> mist.port(1234)
    |> mist.start

  process.sleep_forever()
}

fn serve_html() -> Response(ResponseData) {
  let html =
    html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.title([], "05-server-components/01-basic-setup"),
        tiramisu.script(),
        html.script(
          [attribute.type_("module"), attribute.src("/lustre/runtime.mjs")],
          "",
        ),
        html.script(
          [attribute.type_("module")],
          "import { main } from '/client/app/app.mjs'; main();",
        ),
      ]),
      html.body(
        [
          attribute.styles([
            #("margin", "0"),
            #("font-family", "Iosevka, Menlo, monospace"),
            #("background", "#f7f4ed"),
            #("color", "#111827"),
          ]),
        ],
        [
          html.main(
            [
              attribute.styles([
                #("max-width", "70rem"),
                #("margin", "0 auto"),
                #("padding", "2rem"),
              ]),
            ],
            [
              html.h1([], [html.text("Tiramisu over Lustre server components")]),
              html.p([], [
                html.text(
                  "The scene below is rendered by a Lustre server component. "
                  <> "The browser only hosts the thin Lustre runtime and "
                  <> "registers Tiramisu's custom elements.",
                ),
              ]),
              server_component.element([server_component.route("/ws")], []),
            ],
          ),
        ],
      ),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_body(mist.Bytes(html))
  |> response.set_header("content-type", "text/html; charset=utf-8")
}

fn serve_lustre_runtime() -> Response(ResponseData) {
  let assert Ok(lustre_priv) = application.priv_directory("lustre")
  let file_path = lustre_priv <> "/static/lustre-server-component.mjs"

  case mist.send_file(file_path, offset: 0, limit: None) {
    Ok(file) ->
      response.new(200)
      |> response.prepend_header("content-type", "application/javascript")
      |> response.set_body(file)

    Error(_) -> not_found()
  }
}

fn serve_client_asset(segments: List(String)) -> Response(ResponseData) {
  case has_invalid_path_segment(segments) {
    True -> not_found()
    False -> {
      let file_path =
        "client/build/dev/javascript/" <> string.join(segments, with: "/")

      case mist.send_file(file_path, offset: 0, limit: None) {
        Ok(file) ->
          response.new(200)
          |> response.prepend_header("content-type", content_type(file_path))
          |> response.set_body(file)

        Error(_) -> not_found()
      }
    }
  }
}

fn has_invalid_path_segment(segments: List(String)) -> Bool {
  list.any(segments, fn(segment) {
    segment == "." || segment == ".." || string.contains(segment, "\\")
  })
}

fn content_type(path: String) -> String {
  case string.ends_with(path, ".mjs") || string.ends_with(path, ".js") {
    True -> "application/javascript; charset=utf-8"
    False -> "text/plain; charset=utf-8"
  }
}

fn serve_scene_component(request: Request(Connection)) -> Response(ResponseData) {
  mist.websocket(
    request:,
    on_init: init_scene_socket,
    handler: loop_scene_socket,
    on_close: close_scene_socket,
  )
}

type SceneSocket {
  SceneSocket(
    component: lustre.Runtime(scene_component.Msg),
    self: process.Subject(server_component.ClientMessage(scene_component.Msg)),
  )
}

type SceneSocketMessage =
  server_component.ClientMessage(scene_component.Msg)

fn init_scene_socket(_) {
  let assert Ok(component) =
    lustre.start_server_component(scene_component.component(), Nil)

  let self = process.new_subject()
  let selector =
    process.new_selector()
    |> process.select(self)

  server_component.register_subject(self)
  |> lustre.send(to: component)

  #(SceneSocket(component:, self:), Some(selector))
}

fn loop_scene_socket(
  state: SceneSocket,
  message: mist.WebsocketMessage(SceneSocketMessage),
  connection: mist.WebsocketConnection,
) -> mist.Next(SceneSocket, SceneSocketMessage) {
  case message {
    mist.Text(json_text) -> {
      case json.parse(json_text, server_component.runtime_message_decoder()) {
        Ok(runtime_message) -> lustre.send(state.component, runtime_message)
        Error(_) -> Nil
      }

      mist.continue(state)
    }

    mist.Binary(_) -> mist.continue(state)

    mist.Custom(client_message) -> {
      let json_text =
        client_message
        |> server_component.client_message_to_json
        |> json.to_string
      let assert Ok(_) = mist.send_text_frame(connection, json_text)

      mist.continue(state)
    }

    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

fn close_scene_socket(state: SceneSocket) -> Nil {
  lustre.shutdown()
  |> lustre.send(to: state.component)
}

fn not_found() -> Response(ResponseData) {
  response.new(404)
  |> response.set_body(mist.Bytes(bytes_tree.new()))
}
