import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam_community/ansi
import gleam_community/colour.{type Colour}
import glint/internal/utils

/// Style heading text with the provided rgb colouring
/// this is only intended for use within glint itself.
///
fn heading_style(heading: String, colour: Colour) -> String {
  heading
  |> ansi.bold
  |> ansi.underline
  |> ansi.italic
  |> ansi.hex(colour.to_rgb_hex(colour))
}

// --- HELP: CONSTANTS ---
//
pub const help_flag = Flag(Metadata("help", "Print help information"), "")

const flags_heading = "FLAGS:"

const subcommands_heading = "SUBCOMMANDS:"

const usage_heading = "USAGE:"

// --- HELP: TYPES ---

pub type ArgsCount {
  MinArgs(Int)
  EqArgs(Int)
}

pub type Config {
  Config(
    name: Option(String),
    usage_colour: Option(Colour),
    flags_colour: Option(Colour),
    subcommands_colour: Option(Colour),
    as_module: Bool,
    description: Option(String),
    indent_width: Int,
    max_output_width: Int,
    min_first_column_width: Int,
    column_gap: Int,
    flag_prefix: String,
    flag_delimiter: String,
  )
}

/// Common metadata for commands and flags
///
pub type Metadata {
  Metadata(name: String, description: String)
}

/// Help type for flag metadata
///
pub type Flag {
  Flag(meta: Metadata, type_: String)
}

/// Help type for command metadata
pub type Command {
  Command(
    // Every command has a name and description
    meta: Metadata,
    // A command can have >= 0 flags associated with it
    flags: List(Flag),
    // A command can have >= 0 subcommands associated with it
    subcommands: List(Metadata),
    // A command can have a set number of unnamed arguments
    unnamed_args: Option(ArgsCount),
    // A command can specify named arguments
    named_args: List(String),
  )
}

// -- HELP - FUNCTIONS - STRINGIFIERS --
pub fn command_help_to_string(help: Command, config: Config) -> String {
  let command = case help.meta.name {
    "" -> ""
    s -> "Command: " <> s
  }

  let command_description =
    help.meta.description
    |> utils.wordwrap(config.max_output_width)
    |> string.join("\n")

  [
    config.description |> option.unwrap(""),
    command,
    command_description,
    command_help_to_usage_string(help, config),
    flags_help_to_string(help.flags, config),
    subcommands_help_to_string(help.subcommands, config),
  ]
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n\n")
}

// -- HELP - FUNCTIONS - STRINGIFIERS - USAGE --

/// convert a List(Flag) to a list of strings for use in usage text
///
fn flags_help_to_usage_strings(help: List(Flag), config: Config) -> List(String) {
  help
  |> list.map(flag_help_to_string(_, config))
  |> list.sort(string.compare)
}

/// generate the usage help text for the flags of a command
///
fn flags_help_to_usage_string(config: Config, help: List(Flag)) -> String {
  use <- bool.guard(help == [], "")
  let content =
    help
    |> flags_help_to_usage_strings(config)
    |> string.join(" ")

  "[ " <> content <> " ]"
}

/// convert an ArgsCount to a string for usage text
///
fn args_count_to_usage_string(count: ArgsCount) -> String {
  case count {
    EqArgs(0) -> ""
    EqArgs(1) -> "[ 1 argument ]"
    EqArgs(n) -> "[ " <> int.to_string(n) <> " arguments ]"
    MinArgs(n) -> "[ " <> int.to_string(n) <> " or more arguments ]"
  }
}

/// convert a Command to a styled usage block
///
fn command_help_to_usage_string(help: Command, config: Config) -> String {
  let app_name = case config.name {
    Some(name) if config.as_module -> "gleam run -m " <> name
    Some(name) -> name
    None -> "gleam run"
  }

  let flags = flags_help_to_usage_string(config, help.flags)
  let subcommands = case
    list.map(help.subcommands, fn(sc) { sc.name })
    |> list.sort(string.compare)
    |> string.join(" | ")
  {
    "" -> ""
    subcommands -> "( " <> subcommands <> " )"
  }

  let named_args =
    help.named_args
    |> list.map(fn(s) { "<" <> s <> ">" })
    |> string.join(" ")

  let unnamed_args =
    option.map(help.unnamed_args, args_count_to_usage_string)
    |> option.unwrap("[ ARGS ]")

  // The max width of the usage accounts for the constant indent
  let max_usage_width = config.max_output_width - config.indent_width

  let content =
    [app_name, help.meta.name, subcommands, named_args, unnamed_args, flags]
    |> list.filter(fn(s) { s != "" })
    |> string.join(" ")
    |> utils.wordwrap(max_usage_width)
    |> string.join("\n" <> string.repeat(" ", config.indent_width * 2))

  case config.usage_colour {
    None -> usage_heading
    Some(pretty) -> heading_style(usage_heading, pretty)
  }
  <> "\n"
  <> string.repeat(" ", config.indent_width)
  <> content
}

// -- HELP - FUNCTIONS - STRINGIFIERS - FLAGS --

/// generate the usage help string for a command
///
fn flags_help_to_string(help: List(Flag), config: Config) -> String {
  use <- bool.guard(help == [], "")

  let longest_flag_length =
    help
    |> list.map(flag_help_to_string(_, config))
    |> utils.max_string_length
    |> int.max(config.min_first_column_width)

  let heading = case config.flags_colour {
    None -> flags_heading
    Some(pretty) -> heading_style(flags_heading, pretty)
  }

  let content =
    to_spaced_indented_string(
      [help_flag, ..help],
      fn(help) { #(flag_help_to_string(help, config), help.meta.description) },
      longest_flag_length,
      config,
    )

  heading <> content
}

/// generate the help text for a flag without a description
///
fn flag_help_to_string(help: Flag, config: Config) -> String {
  config.flag_prefix
  <> help.meta.name
  <> case help.type_ {
    "" -> ""
    _ -> config.flag_delimiter <> "<" <> help.type_ <> ">"
  }
}

// -- HELP - FUNCTIONS - STRINGIFIERS - SUBCOMMANDS --

/// generate the styled help text for a list of subcommands
///
fn subcommands_help_to_string(help: List(Metadata), config: Config) -> String {
  use <- bool.guard(help == [], "")

  let longest_subcommand_length =
    help
    |> list.map(fn(h) { h.name })
    |> utils.max_string_length
    |> int.max(config.min_first_column_width)

  let heading = case config.subcommands_colour {
    None -> subcommands_heading
    Some(pretty) -> heading_style(subcommands_heading, pretty)
  }

  let content =
    to_spaced_indented_string(
      help,
      fn(help) { #(help.name, help.description) },
      longest_subcommand_length,
      config,
    )

  heading <> content
}

/// convert a list of items to an indented string with spaced contents
///
fn to_spaced_indented_string(
  // items to be stringified and joined
  data: List(a),
  // function to convert each item to a tuple of (left, right) strings
  f: fn(a) -> #(String, String),
  // longest length of the first column
  left_length: Int,
  // how many spaces to indent each line
  config: Config,
) -> String {
  let left_length = left_length + config.column_gap

  let #(content, wrapped) =
    list.fold(data, #([], False), fn(acc, data) {
      let #(left, right) = f(data)
      let #(line, wrapped) = format_content(left, right, left_length, config)
      #([line, ..acc.0], wrapped || acc.1)
    })

  let joiner = case wrapped {
    True -> "\n"
    False -> ""
  }

  content |> list.sort(string.compare) |> string.join(joiner)
}

fn format_content(
  left: String,
  right: String,
  left_length: Int,
  config: Config,
) -> #(String, Bool) {
  let left_formatted = string.pad_end(left, left_length, " ")

  let lines =
    config.max_output_width
    |> int.subtract(left_length + config.indent_width)
    |> int.max(config.min_first_column_width)
    |> utils.wordwrap(right, _)

  let right_formatted =
    string.join(
      lines,
      "\n" <> string.repeat(" ", config.indent_width + left_length),
    )

  let wrapped = case lines {
    [] | [_] -> False
    _ -> True
  }

  #(
    "\n"
      <> string.repeat(" ", config.indent_width)
      <> left_formatted
      <> right_formatted,
    wrapped,
  )
}
