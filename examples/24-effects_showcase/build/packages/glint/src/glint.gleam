import gleam
import gleam/dict
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam_community/colour.{type Colour}
import glint/constraint
import glint/internal/help
import snag.{type Snag}

// --- CONFIGURATION ---

// -- CONFIGURATION: TYPES --

/// Config for glint
///
type Config {
  Config(
    pretty_help: Option(PrettyHelp),
    name: Option(String),
    as_module: Bool,
    description: Option(String),
    exit: Bool,
    indent_width: Int,
    max_output_width: Int,
    min_first_column_width: Int,
    column_gap: Int,
  )
}

/// PrettyHelp defines the header colours to be used when styling help text
///
pub type PrettyHelp {
  PrettyHelp(usage: Colour, flags: Colour, subcommands: Colour)
}

// -- CONFIGURATION: CONSTANTS --

/// default config
///
const default_config = Config(
  pretty_help: None,
  name: None,
  as_module: False,
  description: None,
  exit: True,
  indent_width: 4,
  max_output_width: 80,
  min_first_column_width: 20,
  column_gap: 2,
)

// -- CONFIGURATION: FUNCTIONS --

/// Enable custom colours for help text headers.
///
/// For a pre-made style, pass in [`glint.default_pretty_help`](#default_pretty_help)
///
pub fn pretty_help(glint: Glint(a), pretty: PrettyHelp) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, pretty_help: Some(pretty)))
}

/// Give the current glint application a name.
///
/// The name specified here is used when generating help text for the current glint instance.
///
pub fn with_name(glint: Glint(a), name: String) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, name: Some(name)))
}

/// By default, Glint exits with error status 1 when an error is encountered (eg. invalid flag or command not found)
///
/// Calling this function disables that feature.
///
pub fn without_exit(glint: Glint(a)) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, exit: False))
}

/// Adjust the generated help text to reflect that the current glint app should be run as a gleam module.
///
/// Use in conjunction with [`glint.with_name`](#with_name) to get usage text output like `gleam run -m <name>`
///
pub fn as_module(glint: Glint(a)) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, as_module: True))
}

/// Adjusts the indent width used to indent content under the usage, flags,
/// and subcommands headings in the help output.
///
/// Default: 4.
///
pub fn with_indent_width(glint: Glint(a), indent_width: Int) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, indent_width:))
}

/// Adjusts the output width at which help text will wrap onto a new line.
///
/// Default: 80.
///
pub fn with_max_output_width(glint: Glint(a), max_output_width: Int) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, max_output_width:))
}

/// Adjusts the minimum width of the column containing flag and command names in the help output.
///
/// Default: 20.
///
pub fn with_min_first_column_width(
  glint: Glint(a),
  min_first_column_width: Int,
) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, min_first_column_width:))
}

/// Adjusts the size of the gap between columns in the help output.
///
/// Default: 2.
///
pub fn with_column_gap(glint: Glint(a), column_gap: Int) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, column_gap:))
}

// --- CORE ---

// -- CORE: TYPES --

/// A container type for config and commands.
///
/// This will be the main interaction point when setting up glint.
/// To create a new one use [`glint.new`](#new).
///
pub opaque type Glint(a) {
  Glint(config: Config, cmd: CommandNode(a))
}

/// Specify the expected number of unnamed arguments with this type and the [`glint.unnamed_args`](#unnamed_args) function
///
pub type ArgsCount {
  /// Specifies that a command must accept a specific number of unnamed arguments
  ///
  EqArgs(Int)
  /// Specifies that a command must accept a minimum number of unnamed arguments
  ///
  MinArgs(Int)
}

/// The type representing a glint command.
///
/// To create a new command, use the [`glint.command`](#command) function.
///
pub opaque type Command(a) {
  Command(
    do: Runner(a),
    flags: Flags,
    description: String,
    unnamed_args: Option(ArgsCount),
    named_args: List(String),
  )
}

type InternalCommand(a) {
  InternalCommand(
    do: Runner(a),
    flags: Flags,
    unnamed_args: Option(ArgsCount),
    named_args: List(String),
  )
}

/// A container for named arguments available to commands at runtime.
///
pub opaque type NamedArgs {
  NamedArgs(internal: dict.Dict(String, String))
}

/// Functions that execute when glint commands are run.
///
pub type Runner(a) =
  fn(NamedArgs, List(String), Flags) -> a

/// CommandNode tree representation.
///
type CommandNode(a) {
  CommandNode(
    contents: Option(InternalCommand(a)),
    subcommands: dict.Dict(String, CommandNode(a)),
    group_flags: Flags,
    description: String,
  )
}

/// Ok type for command execution
///
@internal
pub type Out(a) {
  /// Container for the command return value
  Out(a)
  /// Container for the generated help string
  Help(String)
}

// -- CORE: BUILDER FUNCTIONS --

/// Create a new glint instance.
///
pub fn new() -> Glint(a) {
  Glint(config: default_config, cmd: empty_command())
}

/// Set the help text for a specific command path.
///
/// This function is intended to allow users to set the help text of commands that might not be directly instantiated,
/// such as commands with no business logic associated to them but that have subcommands.
///
/// Using this function should almost never be necessary, in most cases you should use [`glint.command_help`](#command_help) insstead.
pub fn path_help(
  in glint: Glint(a),
  at path: List(String),
  put description: String,
) -> Glint(a) {
  use node <- update_at(in: glint, at: path)
  CommandNode(..node, description: description)
}

/// Set help text for the application as a whole.
///
/// Help text set with this function wil be printed at the top of the help text for every command.
/// To set help text specifically for the root command please use [`glint.command_help`](#command_help) or [`glint.path_help([],...)`](#path_help)
///
/// This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.
/// This is useful for developers to format their help text in a more readable way in the source code.
///
/// For formatted text to appear on a new line, use 2 newline characters.
/// For formatted text to appear in a new paragraph, use 3 newline characters.
///
pub fn global_help(in glint: Glint(a), of description: String) -> Glint(a) {
  Glint(..glint, config: Config(..glint.config, description: Some(description)))
}

/// Adds a new command to be run at the specified path.
///
/// If the path is `[]`, the root command is set with the provided function and
/// flags.
///
/// Note: all command paths are sanitized by stripping whitespace and removing any empty string elements.
///
/// ```gleam
/// glint.new()
/// |> glint.add(at: [], do: root_command())
/// |> glint.add(at: ["subcommand"], do: subcommand())
/// ...
/// ```
///
pub fn add(
  to glint: Glint(a),
  at path: List(String),
  do command: Command(a),
) -> Glint(a) {
  use node <- update_at(in: glint, at: path)
  CommandNode(
    ..node,
    description: command.description,
    contents: Some(InternalCommand(
      do: command.do,
      flags: command.flags,
      named_args: command.named_args,
      unnamed_args: command.unnamed_args,
    )),
  )
}

/// Helper for initializing empty commands
///
fn empty_command() -> CommandNode(a) {
  CommandNode(
    contents: None,
    subcommands: dict.new(),
    group_flags: new_flags(),
    description: "",
  )
}

/// Trim each path element and remove any resulting empty strings.
///
fn sanitize_path(path: List(String)) -> List(String) {
  path
  |> list.map(string.trim)
  |> list.filter(fn(s) { s != "" })
}

/// Create a [Command(a)](#Command) from a [Runner(a)](#Runner).
///
/// ### Example:
///
/// ```gleam
/// use <- glint.command_help("Some awesome help text")
/// use named_arg <- glint.named_arg("some_arg")
/// use <- glint.unnamed_args(glint.EqArgs(0))
/// ...
/// use named, unnamed, flags <- glint.command()
/// let my_arg = named_arg(named)
/// ...
/// ```
pub fn command(do runner: Runner(a)) -> Command(a) {
  Command(
    do: runner,
    flags: new_flags(),
    description: "",
    unnamed_args: None,
    named_args: [],
  )
}

/// Map the output of a [`Command`](#Command)
///
/// This function can be useful when you are handling user-defined commands or commands from other packages and need to make sure the return type matches your own commands.
///
pub fn map_command(command: Command(a), with fun: fn(a) -> b) -> Command(b) {
  Command(
    do: fn(named_args, args, flags) { fun(command.do(named_args, args, flags)) },
    description: command.description,
    flags: command.flags,
    named_args: command.named_args,
    unnamed_args: command.unnamed_args,
  )
}

/// Attach a helptext description to a [`Command(a)`](#Command)
///
/// This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.
/// This is useful for developers to format their help text in a more readable way in the source code.
///
/// For formatted text to appear on a new line, use 2 newline characters.
/// For formatted text to appear in a new paragraph, use 3 newline characters.
///
pub fn command_help(of desc: String, with f: fn() -> Command(a)) -> Command(a) {
  Command(..f(), description: desc)
}

/// Specify a specific number of unnamed args that a given command expects.
///
/// Use in conjunction with [`glint.ArgsCount`](#ArgsCount) to specify either a minimum or a specific number of args.
///
/// ### Example:
///
/// ```gleam
/// ...
/// // for a command that accets only 1 unnamed argument:
/// use <- glint.unnamed_args(glint.EqArgs(1))
/// ...
/// named, unnamed, flags <- glint.command()
/// let assert Ok([arg]) = unnamed
/// ```
///
pub fn unnamed_args(
  of count: ArgsCount,
  with f: fn() -> Command(b),
) -> Command(b) {
  Command(..f(), unnamed_args: Some(count))
}

/// Add a list of named arguments to a [`Command(a)`](#Command). The value can be retrieved from the command's [`NamedArgs`](#NamedArgs)
///
/// These named arguments will be matched with the first N arguments passed to the command.
///
///
/// **IMPORTANT**:
///
/// - Matched named arguments will **not** be present in the commmand's unnamed args list
///
/// - All named arguments must match for a command to succeed.
///
/// ### Example:
///
/// ```gleam
/// ...
/// use first_name <- glint.named_arg("first name")
/// ...
/// use named, unnamed, flags <- glint.command()
/// let first = first_name(named)
/// ```
///
pub fn named_arg(
  named name: String,
  with f: fn(fn(NamedArgs) -> String) -> Command(a),
) -> Command(a) {
  let cmd = {
    use named_args <- f()
    // we can assert here because the command runner will only execute if the named args match
    let assert Ok(arg) = dict.get(named_args.internal, name)
    arg
  }

  Command(..cmd, named_args: [name, ..cmd.named_args])
}

/// Add a [`Flag(a)`](#Flag) to a [`Command(a)`](#Command)
///
/// The provided callback is provided a function to fetch the current flag fvalue from the command input [`Flags`](#Flags).
///
/// This function is most ergonomic as part of a `use` chain when building commands.
///
/// ### Example:
///
/// ```gleam
/// ...
/// use repeat <- glint.flag(
///   glint.int_flag("repeat")
///   |> glint.flag_default(1)
///   |> glint.flag_help("Repeat the message n-times")
/// )
/// ...
/// use named, unnamed, flags <- glint.command()
/// let repeat_value = repeat(flags)
/// ```
///
pub fn flag(
  of flag: Flag(a),
  with f: fn(fn(Flags) -> snag.Result(a)) -> Command(b),
) -> Command(b) {
  let cmd = f(flag.getter(_, flag.name))
  Command(..cmd, flags: insert(cmd.flags, flag.name, build_flag(flag)))
}

/// Add a flag for a group of commands.
/// The provided flags will be available to all commands at or beyond the provided path
///
pub fn group_flag(
  in glint: Glint(a),
  at path: List(String),
  of flag: Flag(_),
) -> Glint(a) {
  use node <- update_at(in: glint, at: path)
  CommandNode(
    ..node,
    group_flags: insert(node.group_flags, flag.name, build_flag(flag)),
  )
}

// -- CORE: EXECUTION FUNCTIONS --

/// Determines which command to run and executes it.
///
/// Sets any provided flags if necessary.
///
/// Each value prefixed with `--` is parsed as a flag.
///
/// This function does not print its output and is mainly intended for use within `glint` itself.
/// If you would like to print or handle the output of a command please see the `run_and_handle` function.
///
@internal
pub fn execute(glint: Glint(a), args: List(String)) -> Result(Out(a), String) {
  // create help flag to check for
  let help_flag = flag_prefix <> help.help_flag.meta.name

  // check if help flag is present
  let #(help, args) = case list.partition(args, fn(s) { s == help_flag }) {
    // help flag not in args
    #([], args) -> #(False, args)
    // help flag in args
    #(_, args) -> #(True, args)
  }

  // split flags out from the args list
  let #(flags, args) = list.partition(args, string.starts_with(_, flag_prefix))

  // search for command and execute
  do_execute(glint.cmd, glint.config, args, flags, help, [])
}

/// Find which command to execute and run it with computed flags and args
///
fn do_execute(
  cmd: CommandNode(a),
  config: Config,
  args: List(String),
  flags: List(String),
  help: Bool,
  command_path: List(String),
) -> Result(Out(a), String) {
  case args {
    // when there are no more available arguments
    // and help flag has been passed, generate help message
    [] if help -> Ok(Help(cmd_help(command_path, cmd, config)))

    // when there are no more available arguments
    // run the current command
    [] -> execute_root(command_path, config, cmd, [], flags) |> result.map(Out)

    // when there are arguments remaining
    // check if the next one is a subcommand of the current command
    [arg, ..rest] ->
      case dict.get(cmd.subcommands, arg) {
        // subcommand found, continue
        Ok(sub_command) ->
          CommandNode(
            ..sub_command,
            group_flags: merge(cmd.group_flags, sub_command.group_flags),
          )
          |> do_execute(config, rest, flags, help, [arg, ..command_path])

        // subcommand not found, but help flag has been passed
        // generate and return help message
        _ if help -> Ok(Help(cmd_help(command_path, cmd, config)))

        // subcommand not found, but help flag has not been passed
        // execute the current command
        _ ->
          execute_root(command_path, config, cmd, args, flags)
          |> result.map(Out)
      }
  }
}

fn args_compare(expected: ArgsCount, actual: Int) -> snag.Result(Nil) {
  use err <- result.map_error(case expected {
    EqArgs(expected) if actual == expected -> Ok(Nil)
    MinArgs(expected) if actual >= expected -> Ok(Nil)
    EqArgs(expected) -> Error(int.to_string(expected))
    MinArgs(expected) -> Error("at least " <> int.to_string(expected))
  })
  snag.new(
    "expected: " <> err <> " argument(s), provided: " <> int.to_string(actual),
  )
}

/// Executes the current root command.
///
fn execute_root(
  path: List(String),
  config: Config,
  cmd: CommandNode(a),
  args: List(String),
  flag_inputs: List(String),
) -> Result(a, String) {
  {
    // check if the command can actually be executed
    use contents <- result.try(option.to_result(
      cmd.contents,
      snag.new("command not found"),
    ))

    // merge flags and parse flag inputs
    use new_flags <- result.try(list.try_fold(
      over: flag_inputs,
      from: merge(cmd.group_flags, contents.flags),
      with: update_flags,
    ))

    // get named arguments
    use named_args <- result.try({
      let named = list.zip(contents.named_args, args)
      case list.length(named) == list.length(contents.named_args) {
        True -> Ok(dict.from_list(named))
        False ->
          snag.error(
            "unmatched named arguments: "
            <> {
              contents.named_args
              |> list.drop(list.length(named))
              |> list.map(fn(s) { "'" <> s <> "'" })
              |> string.join(", ")
            },
          )
      }
    })

    // get unnamed arguments
    let args = list.drop(args, dict.size(named_args))

    // validate unnamed argument quantity
    use _ <- result.map(case contents.unnamed_args {
      Some(count) ->
        count
        |> args_compare(list.length(args))
        |> snag.context("invalid number of arguments provided")
      None -> Ok(Nil)
    })

    // execute the command
    contents.do(NamedArgs(named_args), args, new_flags)
  }
  |> result.map_error(fn(err) {
    err
    |> snag.layer("failed to run command")
    |> snag.pretty_print
    <> "\nSee the following help text, available via the '--help' flag.\n\n"
    <> cmd_help(path, cmd, config)
  })
}

/// Run a glint app and print any errors enountered, or the help text if requested.
/// This function ignores any value returned by the command that was run.
/// If you would like to do handle the command output please see the [`glint.run_and_handle`](#run_and_handle) function.
///
/// IMPORTANT: This function exits with code 1 if an error was encountered.
/// If this behaviour is not desired please disable it with [`glint.without_exit`](#without_exit)
///
pub fn run(from glint: Glint(a), for args: List(String)) -> Nil {
  run_and_handle(from: glint, for: args, with: fn(_) { Nil })
}

/// Run a glint app with a custom handler for command output.
/// This function prints any errors enountered or the help text if requested.
///
/// IMPORTANT: This function exits with code 1 if an error was encountered.
/// If this behaviour is not desired please disable it with [`glint.without_exit`](#without_exit)
///
pub fn run_and_handle(
  from glint: Glint(a),
  for args: List(String),
  with handle: fn(a) -> _,
) -> Nil {
  case execute(glint, args) {
    Error(s) -> {
      io.println(s)
      case glint.config.exit {
        True -> exit(1)
        False -> Nil
      }
    }
    Ok(Help(s)) -> io.println(s)
    Ok(Out(out)) -> {
      handle(out)
      Nil
    }
  }
}

/// Default colouring for help text.
///
/// mint (r: 182, g: 255, b: 234) colour for usage
///
/// pink (r: 255, g: 175, b: 243) colour for flags
///
/// buttercup (r: 252, g: 226, b: 174) colour for subcommands
///
pub fn default_pretty_help() -> PrettyHelp {
  let assert Ok(usage_colour) = colour.from_rgb255(182, 255, 234)
  let assert Ok(flags_colour) = colour.from_rgb255(255, 175, 243)
  let assert Ok(subcommands_colour) = colour.from_rgb255(252, 226, 174)

  PrettyHelp(
    usage: usage_colour,
    flags: flags_colour,
    subcommands: subcommands_colour,
  )
}

// -- HELP: FUNCTIONS --

/// generate the help text for a command
fn cmd_help(path: List(String), cmd: CommandNode(a), config: Config) -> String {
  // recreate the path of the current command
  // reverse the path because it is created by prepending each section as do_execute walks down the tree
  path
  |> list.reverse
  |> string.join(" ")
  |> build_command_help(cmd)
  |> help.command_help_to_string(build_help_config(config))
}

// -- HELP - FUNCTIONS - BUILDERS --
fn build_help_config(config: Config) -> help.Config {
  help.Config(
    name: config.name,
    usage_colour: option.map(config.pretty_help, fn(p) { p.usage }),
    flags_colour: option.map(config.pretty_help, fn(p) { p.flags }),
    subcommands_colour: option.map(config.pretty_help, fn(p) { p.subcommands }),
    as_module: config.as_module,
    description: config.description,
    indent_width: config.indent_width,
    max_output_width: config.max_output_width,
    min_first_column_width: config.min_first_column_width,
    column_gap: config.column_gap,
    flag_prefix: flag_prefix,
    flag_delimiter: flag_delimiter,
  )
}

/// build the help representation for a subtree of commands
///
fn build_command_help(name: String, node: CommandNode(_)) -> help.Command {
  let #(description, flags, unnamed_args, named_args) =
    node.contents
    |> option.map(fn(cmd) {
      #(
        node.description,
        build_flags_help(merge(node.group_flags, cmd.flags)),
        cmd.unnamed_args,
        cmd.named_args,
      )
    })
    |> option.unwrap(#(node.description, [], None, []))

  help.Command(
    meta: help.Metadata(name: name, description: description),
    flags: flags,
    subcommands: build_subcommands_help(node.subcommands),
    unnamed_args: {
      use args <- option.map(unnamed_args)
      case args {
        EqArgs(n) -> help.EqArgs(n)
        MinArgs(n) -> help.MinArgs(n)
      }
    },
    named_args: named_args,
  )
}

/// generate the string representation for the type of a flag
///
fn flag_type_info(flag: FlagEntry) {
  case flag.value {
    I(_) -> "INT"
    B(_) -> "BOOL"
    F(_) -> "FLOAT"
    LF(_) -> "FLOAT_LIST"
    LI(_) -> "INT_LIST"
    LS(_) -> "STRING_LIST"
    S(_) -> "STRING"
  }
}

/// build the help representation for a list of flags
///
fn build_flags_help(flags: Flags) -> List(help.Flag) {
  use acc, name, flag <- fold(flags, [])
  [
    help.Flag(
      meta: help.Metadata(name: name, description: flag.description),
      type_: flag_type_info(flag),
    ),
    ..acc
  ]
}

/// build the help representation for a list of subcommands
///
fn build_subcommands_help(
  subcommands: dict.Dict(String, CommandNode(_)),
) -> List(help.Metadata) {
  use acc, name, node <- dict.fold(subcommands, [])
  [help.Metadata(name: name, description: node.description), ..acc]
}

// ----- FLAGS -----

/// FlagEntry inputs must start with this prefix
///
const flag_prefix = "--"

/// The separation character for flag names and their values
const flag_delimiter = "="

/// Supported flag types.
///
type Value {
  /// Boolean flags, to be passed in as `--flag=true` or `--flag=false`.
  /// Can be toggled by omitting the desired value like `--flag`.
  /// Toggling will negate the existing value.
  ///
  B(FlagInternals(Bool))

  /// Int flags, to be passed in as `--flag=1`
  ///
  I(FlagInternals(Int))

  /// List(Int) flags, to be passed in as `--flag=1,2,3`
  ///
  LI(FlagInternals(List(Int)))

  /// Float flags, to be passed in as `--flag=1.0`
  ///
  F(FlagInternals(Float))

  /// List(Float) flags, to be passed in as `--flag=1.0,2.0`
  ///
  LF(FlagInternals(List(Float)))

  /// String flags, to be passed in as `--flag=hello`
  ///
  S(FlagInternals(String))

  /// List(String) flags, to be passed in as `--flag=hello,world`
  ///
  LS(FlagInternals(List(String)))
}

/// Glint's typed flags.
///
/// Flags can be created using any of:
/// - [`glint.int_flag`](#int_flag)
/// - [`glint.ints_flag`](#ints_flag)
/// - [`glint.float_flag`](#float_flag)
/// - [`glint.floats_flag`](#floats_flag)
/// - [`glint.string_flag`](#string_flag)
/// - [`glint.strings_flag`](#strings_flag)
/// - [`glint.bool_flag`](#bool_flag)
///
pub opaque type Flag(a) {
  Flag(
    name: String,
    desc: String,
    parser: Parser(a, Snag),
    value: fn(FlagInternals(a)) -> Value,
    getter: fn(Flags, String) -> snag.Result(a),
    default: Option(a),
  )
}

/// An internal representation of flag contents
///
type FlagInternals(a) {
  FlagInternals(value: Option(a), parser: Parser(a, Snag))
}

// Flag initializers

type Parser(a, b) =
  fn(String) -> gleam.Result(a, b)

/// Initialise an int flag.
///
pub fn int_flag(named name: String) -> Flag(Int) {
  use input <- new_builder(name, I, get_int_flag)
  input
  |> int.parse
  |> result.replace_error(cannot_parse(input, "int"))
}

/// Initialise an int list flag.
///
pub fn ints_flag(named name: String) -> Flag(List(Int)) {
  use input <- new_builder(name, LI, get_ints_flag)
  input
  |> string.split(",")
  |> list.try_map(int.parse)
  |> result.replace_error(cannot_parse(input, "int list"))
}

///Initialise a float flag.
///
pub fn float_flag(named name: String) -> Flag(Float) {
  use input <- new_builder(name, F, get_floats)
  input
  |> float.parse
  |> result.replace_error(cannot_parse(input, "float"))
}

/// Initialise a float list flag.
///
pub fn floats_flag(named name: String) -> Flag(List(Float)) {
  use input <- new_builder(name, LF, get_floats_flag)
  input
  |> string.split(",")
  |> list.try_map(float.parse)
  |> result.replace_error(cannot_parse(input, "float list"))
}

/// Initialise a string flag.
///
pub fn string_flag(named name: String) -> Flag(String) {
  new_builder(name, S, get_string_flag, fn(s) { Ok(s) })
}

/// Intitialise a string list flag.
///
pub fn strings_flag(named name: String) -> Flag(List(String)) {
  use input <- new_builder(name, LS, get_strings_flag)
  input
  |> string.split(",")
  |> Ok
}

/// Initialise a boolean flag.
///
pub fn bool_flag(named name: String) -> Flag(Bool) {
  use input <- new_builder(name, B, get_bool_flag)
  case string.lowercase(input) {
    "true" | "t" -> Ok(True)
    "false" | "f" -> Ok(False)
    _ -> Error(cannot_parse(input, "bool"))
  }
}

/// initialize custom builders using a Value constructor and a parsing function
///
fn new_builder(
  name: String,
  valuer: fn(FlagInternals(a)) -> Value,
  getter: fn(Flags, String) -> snag.Result(a),
  p: Parser(a, Snag),
) -> Flag(a) {
  Flag(
    name: name,
    desc: "",
    parser: p,
    value: valuer,
    default: None,
    getter: getter,
  )
}

/// convert a (Flag(a) into its corresponding FlagEntry representation
///
fn build_flag(fb: Flag(a)) -> FlagEntry {
  FlagEntry(
    value: fb.value(FlagInternals(value: fb.default, parser: fb.parser)),
    description: fb.desc,
  )
}

/// Attach a constraint to a flag.
///
/// As constraints are just functions, this works well as both part of a pipeline or with `use`.
///
///
/// ### Pipe:
///
/// ```gleam
/// glint.int_flag("my_flag")
/// |> glint.flag_help("An awesome flag")
/// |> glint.flag_constraint(fn(i) {
///   case i < 0 {
///     True -> snag.error("must be greater than 0")
///     False -> Ok(i)
///   }})
/// ```
///
/// ### Use:
///
/// ```gleam
/// use i <- glint.flag_constraint(
///   glint.int_flag("my_flag")
///   |> glint.flag_help("An awesome flag")
/// )
/// case i < 0 {
///   True -> snag.error("must be greater than 0")
///   False -> Ok(i)
/// }
/// ```
///
pub fn flag_constraint(
  builder: Flag(a),
  constraint: constraint.Constraint(a),
) -> Flag(a) {
  Flag(..builder, parser: wrap_with_constraint(builder.parser, constraint))
}

/// attach a Constraint(a) to a Parser(a,Snag)
/// this function should not be used directly unless
fn wrap_with_constraint(
  p: Parser(a, Snag),
  constraint: constraint.Constraint(a),
) -> Parser(a, Snag) {
  fn(input: String) -> snag.Result(a) { attempt(p(input), constraint) }
}

fn attempt(
  val: gleam.Result(a, e),
  f: fn(a) -> gleam.Result(_, e),
) -> gleam.Result(a, e) {
  use a <- result.try(val)
  result.replace(f(a), a)
}

/// FlagEntry data and descriptions.
///
type FlagEntry {
  FlagEntry(value: Value, description: String)
}

/// Attach a help text description to a flag.
///
/// This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.
/// This is useful for developers to format their help text in a more readable way in the source code.
///
/// For formatted text to appear on a new line, use 2 newline characters.
/// For formatted text to appear in a new paragraph, use 3 newline characters.
///
/// ### Example:
///
/// ```gleam
/// glint.int_flag("awesome_flag")
/// |> glint.flag_help("Some great text!")
/// ```
///
pub fn flag_help(for flag: Flag(a), of description: String) -> Flag(a) {
  Flag(..flag, desc: description)
}

/// Set the default value for a flag.
///
/// ### Example:
///
/// ```gleam
/// glint.int_flag("awesome_flag")
/// |> glint.flag_default(1)
/// ```
///
pub fn flag_default(for flag: Flag(a), of default: a) -> Flag(a) {
  Flag(..flag, default: Some(default))
}

/// Flags passed as input to a command.
///
pub opaque type Flags {
  Flags(internal: dict.Dict(String, FlagEntry))
}

fn insert(in flags: Flags, at name: String, insert flag: FlagEntry) -> Flags {
  Flags(dict.insert(flags.internal, name, flag))
}

fn merge(into a: Flags, from b: Flags) -> Flags {
  Flags(internal: dict.merge(a.internal, b.internal))
}

fn fold(flags: Flags, acc: acc, f: fn(acc, String, FlagEntry) -> acc) -> acc {
  dict.fold(flags.internal, acc, f)
}

fn new_flags() -> Flags {
  Flags(dict.new())
}

/// Updates a flag value, ensuring that the new value can satisfy the required type.
/// Assumes that all flag inputs passed in start with --
/// This function is only intended to be used from glint.execute_root
///
fn update_flags(in flags: Flags, with flag_input: String) -> snag.Result(Flags) {
  let flag_input = string.drop_start(flag_input, string.length(flag_prefix))

  case string.split_once(flag_input, flag_delimiter) {
    Ok(data) -> update_flag_value(flags, data)
    Error(_) -> attempt_toggle_flag(flags, flag_input)
  }
}

fn update_flag_value(
  in flags: Flags,
  with data: #(String, String),
) -> snag.Result(Flags) {
  let #(key, input) = data
  use contents <- result.try(get(flags, key))
  use value <- result.map(
    compute_flag(with: input, given: contents.value)
    |> result.map_error(layer_invalid_flag(_, key)),
  )
  insert(flags, key, FlagEntry(..contents, value: value))
}

fn attempt_toggle_flag(in flags: Flags, at key: String) -> snag.Result(Flags) {
  use contents <- result.try(get(flags, key))
  case contents.value {
    B(FlagInternals(None, ..) as internal) ->
      FlagInternals(..internal, value: Some(True))
      |> B
      |> fn(val) { FlagEntry(..contents, value: val) }
      |> dict.insert(into: flags.internal, for: key)
      |> Flags
      |> Ok
    B(FlagInternals(Some(val), ..) as internal) ->
      FlagInternals(..internal, value: Some(!val))
      |> B
      |> fn(val) { FlagEntry(..contents, value: val) }
      |> dict.insert(into: flags.internal, for: key)
      |> Flags
      |> Ok
    _ -> Error(no_value_flag_err(key))
  }
}

fn access_type_error(flag_type) {
  snag.error("cannot access flag as " <> flag_type)
}

fn flag_not_provided_error() {
  snag.error("no value provided")
}

fn construct_value(
  input: String,
  internal: FlagInternals(a),
  constructor: fn(FlagInternals(a)) -> Value,
) -> snag.Result(Value) {
  use val <- result.map(internal.parser(input))
  constructor(FlagInternals(..internal, value: Some(val)))
}

/// Computes the new flag value given the input and the expected flag type
///
fn compute_flag(with input: String, given current: Value) -> snag.Result(Value) {
  input
  |> case current {
    I(internal) -> construct_value(_, internal, I)
    LI(internal) -> construct_value(_, internal, LI)
    F(internal) -> construct_value(_, internal, F)
    LF(internal) -> construct_value(_, internal, LF)
    S(internal) -> construct_value(_, internal, S)
    LS(internal) -> construct_value(_, internal, LS)
    B(internal) -> construct_value(_, internal, B)
  }
  |> snag.context("failed to compute value for flag")
}

// Error creation and manipulation functions
fn layer_invalid_flag(err: Snag, flag: String) -> Snag {
  snag.layer(err, "invalid flag '" <> flag <> "'")
}

fn no_value_flag_err(flag_input: String) -> Snag {
  { "flag '" <> flag_input <> "' has no assigned value" }
  |> snag.new()
  |> layer_invalid_flag(flag_input)
}

fn undefined_flag_err(key: String) -> Snag {
  "flag provided but not defined"
  |> snag.new()
  |> layer_invalid_flag(key)
}

fn cannot_parse(with value: String, is kind: String) -> Snag {
  { "cannot parse value '" <> value <> "' as " <> kind }
  |> snag.new()
}

// -- FLAG ACCESS FUNCTIONS --

/// Access the contents for the associated flag
///
fn get(flags: Flags, name: String) -> snag.Result(FlagEntry) {
  dict.get(flags.internal, name)
  |> result.replace_error(undefined_flag_err(name))
}

fn get_value(
  from flags: Flags,
  at key: String,
  expecting kind: fn(FlagEntry) -> snag.Result(a),
) -> snag.Result(a) {
  get(flags, key)
  |> result.try(kind)
  |> snag.context("failed to retrieve value for flag '" <> key <> "'")
}

/// Gets the value for the associated flag.
///
/// This function should only ever be used when fetching flags set at the group level.
/// For local flags please use the getter functions provided when calling [`glint.flag`](#flag).
///
pub fn get_flag(from flags: Flags, for flag: Flag(a)) -> snag.Result(a) {
  flag.getter(flags, flag.name)
}

/// Gets the current value for the associated int flag
///
fn get_int_flag(from flags: Flags, for name: String) -> snag.Result(Int) {
  use flag <- get_value(flags, name)
  case flag.value {
    I(FlagInternals(value: Some(val), ..)) -> Ok(val)
    I(FlagInternals(value: None, ..)) -> flag_not_provided_error()
    _ -> access_type_error("int")
  }
}

/// Gets the current value for the associated ints flag
///
fn get_ints_flag(from flags: Flags, for name: String) -> snag.Result(List(Int)) {
  use flag <- get_value(flags, name)
  case flag.value {
    LI(FlagInternals(value: Some(val), ..)) -> Ok(val)
    LI(FlagInternals(value: None, ..)) -> flag_not_provided_error()
    _ -> access_type_error("int list")
  }
}

/// Gets the current value for the associated bool flag
///
fn get_bool_flag(from flags: Flags, for name: String) -> snag.Result(Bool) {
  use flag <- get_value(flags, name)
  case flag.value {
    B(FlagInternals(Some(val), ..)) -> Ok(val)
    B(FlagInternals(None, ..)) -> flag_not_provided_error()
    _ -> access_type_error("bool")
  }
}

/// Gets the current value for the associated string flag
///
fn get_string_flag(from flags: Flags, for name: String) -> snag.Result(String) {
  use flag <- get_value(flags, name)
  case flag.value {
    S(FlagInternals(value: Some(val), ..)) -> Ok(val)
    S(FlagInternals(value: None, ..)) -> flag_not_provided_error()
    _ -> access_type_error("string")
  }
}

/// Gets the current value for the associated strings flag
///
fn get_strings_flag(
  from flags: Flags,
  for name: String,
) -> snag.Result(List(String)) {
  use flag <- get_value(flags, name)
  case flag.value {
    LS(FlagInternals(value: Some(val), ..)) -> Ok(val)
    LS(FlagInternals(value: None, ..)) -> flag_not_provided_error()
    _ -> access_type_error("string list")
  }
}

/// Gets the current value for the associated float flag
///
fn get_floats(from flags: Flags, for name: String) -> snag.Result(Float) {
  use flag <- get_value(flags, name)
  case flag.value {
    F(FlagInternals(value: Some(val), ..)) -> Ok(val)
    F(FlagInternals(value: None, ..)) -> flag_not_provided_error()
    _ -> access_type_error("float")
  }
}

/// Gets the current value for the associated float flag
///
fn get_floats_flag(
  from flags: Flags,
  for name: String,
) -> snag.Result(List(Float)) {
  use flag <- get_value(flags, name)
  case flag.value {
    LF(FlagInternals(value: Some(val), ..)) -> Ok(val)
    LF(FlagInternals(value: None, ..)) -> flag_not_provided_error()
    _ -> access_type_error("float list")
  }
}

// traverses a Glint(a) tree for the provided path
// executes the provided function on the terminal node
//
fn update_at(
  in glint: Glint(a),
  at path: List(String),
  do f: fn(CommandNode(a)) -> CommandNode(a),
) -> Glint(a) {
  Glint(
    ..glint,
    cmd: do_update_at(through: glint.cmd, at: sanitize_path(path), do: f),
  )
}

fn do_update_at(
  through node: CommandNode(a),
  at path: List(String),
  do f: fn(CommandNode(a)) -> CommandNode(a),
) -> CommandNode(a) {
  case path {
    [] -> f(node)
    [next, ..rest] -> {
      CommandNode(..node, subcommands: {
        use found <- dict.upsert(node.subcommands, next)
        found
        |> option.lazy_unwrap(empty_command)
        |> do_update_at(rest, f)
      })
    }
  }
}

@external(erlang, "erlang", "halt")
@external(javascript, "node:process", "exit")
fn exit(status: Int) -> Nil
