import * as $colour from "../gleam_community_colour/gleam_community/colour.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $float from "../gleam_stdlib/gleam/float.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $snag from "../snag/snag.mjs";
import * as $gleam from "./gleam.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "./gleam.mjs";
import * as $constraint from "./glint/constraint.mjs";
import * as $help from "./glint/internal/help.mjs";
import { exit } from "node:process";

const FILEPATH = "src/glint.gleam";

class Config extends $CustomType {
  constructor(pretty_help, name, as_module, description, exit, indent_width, max_output_width, min_first_column_width, column_gap) {
    super();
    this.pretty_help = pretty_help;
    this.name = name;
    this.as_module = as_module;
    this.description = description;
    this.exit = exit;
    this.indent_width = indent_width;
    this.max_output_width = max_output_width;
    this.min_first_column_width = min_first_column_width;
    this.column_gap = column_gap;
  }
}

export class PrettyHelp extends $CustomType {
  constructor(usage, flags, subcommands) {
    super();
    this.usage = usage;
    this.flags = flags;
    this.subcommands = subcommands;
  }
}

class Glint extends $CustomType {
  constructor(config, cmd) {
    super();
    this.config = config;
    this.cmd = cmd;
  }
}

/**
 * Specifies that a command must accept a specific number of unnamed arguments
 */
export class EqArgs extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Specifies that a command must accept a minimum number of unnamed arguments
 */
export class MinArgs extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Command extends $CustomType {
  constructor(do$, flags, description, unnamed_args, named_args) {
    super();
    this.do = do$;
    this.flags = flags;
    this.description = description;
    this.unnamed_args = unnamed_args;
    this.named_args = named_args;
  }
}

class InternalCommand extends $CustomType {
  constructor(do$, flags, unnamed_args, named_args) {
    super();
    this.do = do$;
    this.flags = flags;
    this.unnamed_args = unnamed_args;
    this.named_args = named_args;
  }
}

class NamedArgs extends $CustomType {
  constructor(internal) {
    super();
    this.internal = internal;
  }
}

class CommandNode extends $CustomType {
  constructor(contents, subcommands, group_flags, description) {
    super();
    this.contents = contents;
    this.subcommands = subcommands;
    this.group_flags = group_flags;
    this.description = description;
  }
}

/**
 * Container for the command return value
 * 
 * @ignore
 */
export class Out extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Container for the generated help string
 * 
 * @ignore
 */
export class Help extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Boolean flags, to be passed in as `--flag=true` or `--flag=false`.
 * Can be toggled by omitting the desired value like `--flag`.
 * Toggling will negate the existing value.
 * 
 * @ignore
 */
class B extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Int flags, to be passed in as `--flag=1`
 * 
 * @ignore
 */
class I extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * List(Int) flags, to be passed in as `--flag=1,2,3`
 * 
 * @ignore
 */
class LI extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * Float flags, to be passed in as `--flag=1.0`
 * 
 * @ignore
 */
class F extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * List(Float) flags, to be passed in as `--flag=1.0,2.0`
 * 
 * @ignore
 */
class LF extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * String flags, to be passed in as `--flag=hello`
 * 
 * @ignore
 */
class S extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

/**
 * List(String) flags, to be passed in as `--flag=hello,world`
 * 
 * @ignore
 */
class LS extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

class Flag extends $CustomType {
  constructor(name, desc, parser, value, getter, default$) {
    super();
    this.name = name;
    this.desc = desc;
    this.parser = parser;
    this.value = value;
    this.getter = getter;
    this.default = default$;
  }
}

class FlagInternals extends $CustomType {
  constructor(value, parser) {
    super();
    this.value = value;
    this.parser = parser;
  }
}

class FlagEntry extends $CustomType {
  constructor(value, description) {
    super();
    this.value = value;
    this.description = description;
  }
}

class Flags extends $CustomType {
  constructor(internal) {
    super();
    this.internal = internal;
  }
}

/**
 * Enable custom colours for help text headers.
 *
 * For a pre-made style, pass in [`glint.default_pretty_help`](#default_pretty_help)
 */
export function pretty_help(glint, pretty) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        new Some(pretty),
        _record.name,
        _record.as_module,
        _record.description,
        _record.exit,
        _record.indent_width,
        _record.max_output_width,
        _record.min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Give the current glint application a name.
 *
 * The name specified here is used when generating help text for the current glint instance.
 */
export function with_name(glint, name) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        new Some(name),
        _record.as_module,
        _record.description,
        _record.exit,
        _record.indent_width,
        _record.max_output_width,
        _record.min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * By default, Glint exits with error status 1 when an error is encountered (eg. invalid flag or command not found)
 *
 * Calling this function disables that feature.
 */
export function without_exit(glint) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        _record.name,
        _record.as_module,
        _record.description,
        false,
        _record.indent_width,
        _record.max_output_width,
        _record.min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Adjust the generated help text to reflect that the current glint app should be run as a gleam module.
 *
 * Use in conjunction with [`glint.with_name`](#with_name) to get usage text output like `gleam run -m <name>`
 */
export function as_module(glint) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        _record.name,
        true,
        _record.description,
        _record.exit,
        _record.indent_width,
        _record.max_output_width,
        _record.min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Adjusts the indent width used to indent content under the usage, flags,
 * and subcommands headings in the help output.
 *
 * Default: 4.
 */
export function with_indent_width(glint, indent_width) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        _record.name,
        _record.as_module,
        _record.description,
        _record.exit,
        indent_width,
        _record.max_output_width,
        _record.min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Adjusts the output width at which help text will wrap onto a new line.
 *
 * Default: 80.
 */
export function with_max_output_width(glint, max_output_width) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        _record.name,
        _record.as_module,
        _record.description,
        _record.exit,
        _record.indent_width,
        max_output_width,
        _record.min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Adjusts the minimum width of the column containing flag and command names in the help output.
 *
 * Default: 20.
 */
export function with_min_first_column_width(glint, min_first_column_width) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        _record.name,
        _record.as_module,
        _record.description,
        _record.exit,
        _record.indent_width,
        _record.max_output_width,
        min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Adjusts the size of the gap between columns in the help output.
 *
 * Default: 2.
 */
export function with_column_gap(glint, column_gap) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        _record.name,
        _record.as_module,
        _record.description,
        _record.exit,
        _record.indent_width,
        _record.max_output_width,
        _record.min_first_column_width,
        column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Set help text for the application as a whole.
 *
 * Help text set with this function wil be printed at the top of the help text for every command.
 * To set help text specifically for the root command please use [`glint.command_help`](#command_help) or [`glint.path_help([],...)`](#path_help)
 *
 * This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.
 * This is useful for developers to format their help text in a more readable way in the source code.
 *
 * For formatted text to appear on a new line, use 2 newline characters.
 * For formatted text to appear in a new paragraph, use 3 newline characters.
 */
export function global_help(glint, description) {
  return new Glint(
    (() => {
      let _record = glint.config;
      return new Config(
        _record.pretty_help,
        _record.name,
        _record.as_module,
        new Some(description),
        _record.exit,
        _record.indent_width,
        _record.max_output_width,
        _record.min_first_column_width,
        _record.column_gap,
      );
    })(),
    glint.cmd,
  );
}

/**
 * Trim each path element and remove any resulting empty strings.
 * 
 * @ignore
 */
function sanitize_path(path) {
  let _pipe = path;
  let _pipe$1 = $list.map(_pipe, $string.trim);
  return $list.filter(_pipe$1, (s) => { return s !== ""; });
}

/**
 * Map the output of a [`Command`](#Command)
 *
 * This function can be useful when you are handling user-defined commands or commands from other packages and need to make sure the return type matches your own commands.
 */
export function map_command(command, fun) {
  return new Command(
    (named_args, args, flags) => {
      return fun(command.do(named_args, args, flags));
    },
    command.flags,
    command.description,
    command.unnamed_args,
    command.named_args,
  );
}

/**
 * Attach a helptext description to a [`Command(a)`](#Command)
 *
 * This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.
 * This is useful for developers to format their help text in a more readable way in the source code.
 *
 * For formatted text to appear on a new line, use 2 newline characters.
 * For formatted text to appear in a new paragraph, use 3 newline characters.
 */
export function command_help(desc, f) {
  let _record = f();
  return new Command(
    _record.do,
    _record.flags,
    desc,
    _record.unnamed_args,
    _record.named_args,
  );
}

/**
 * Specify a specific number of unnamed args that a given command expects.
 *
 * Use in conjunction with [`glint.ArgsCount`](#ArgsCount) to specify either a minimum or a specific number of args.
 *
 * ### Example:
 *
 * ```gleam
 * ...
 * // for a command that accets only 1 unnamed argument:
 * use <- glint.unnamed_args(glint.EqArgs(1))
 * ...
 * named, unnamed, flags <- glint.command()
 * let assert Ok([arg]) = unnamed
 * ```
 */
export function unnamed_args(count, f) {
  let _record = f();
  return new Command(
    _record.do,
    _record.flags,
    _record.description,
    new Some(count),
    _record.named_args,
  );
}

/**
 * Add a list of named arguments to a [`Command(a)`](#Command). The value can be retrieved from the command's [`NamedArgs`](#NamedArgs)
 *
 * These named arguments will be matched with the first N arguments passed to the command.
 *
 *
 * **IMPORTANT**:
 *
 * - Matched named arguments will **not** be present in the commmand's unnamed args list
 *
 * - All named arguments must match for a command to succeed.
 *
 * ### Example:
 *
 * ```gleam
 * ...
 * use first_name <- glint.named_arg("first name")
 * ...
 * use named, unnamed, flags <- glint.command()
 * let first = first_name(named)
 * ```
 */
export function named_arg(name, f) {
  let cmd = f(
    (named_args) => {
      let $ = $dict.get(named_args.internal, name);
      let arg;
      if ($ instanceof Ok) {
        arg = $[0];
      } else {
        throw makeError(
          "let_assert",
          FILEPATH,
          "glint",
          394,
          "named_arg",
          "Pattern match failed, no pattern matched the value.",
          {
            value: $,
            start: 11174,
            end: 11230,
            pattern_start: 11185,
            pattern_end: 11192
          }
        )
      }
      return arg;
    },
  );
  return new Command(
    cmd.do,
    cmd.flags,
    cmd.description,
    cmd.unnamed_args,
    listPrepend(name, cmd.named_args),
  );
}

function args_compare(expected, actual) {
  return $result.map_error(
    (() => {
      if (expected instanceof EqArgs) {
        let expected$1 = expected[0];
        if (actual === expected$1) {
          return new Ok(undefined);
        } else {
          let expected$2 = expected[0];
          return new Error($int.to_string(expected$2));
        }
      } else {
        let expected$1 = expected[0];
        if (actual >= expected$1) {
          return new Ok(undefined);
        } else {
          let expected$2 = expected[0];
          return new Error("at least " + $int.to_string(expected$2));
        }
      }
    })(),
    (err) => {
      return $snag.new$(
        (("expected: " + err) + " argument(s), provided: ") + $int.to_string(
          actual,
        ),
      );
    },
  );
}

/**
 * Default colouring for help text.
 *
 * mint (r: 182, g: 255, b: 234) colour for usage
 *
 * pink (r: 255, g: 175, b: 243) colour for flags
 *
 * buttercup (r: 252, g: 226, b: 174) colour for subcommands
 */
export function default_pretty_help() {
  let $ = $colour.from_rgb255(182, 255, 234);
  let usage_colour;
  if ($ instanceof Ok) {
    usage_colour = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "glint",
      643,
      "default_pretty_help",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 18767,
        end: 18830,
        pattern_start: 18778,
        pattern_end: 18794
      }
    )
  }
  let $1 = $colour.from_rgb255(255, 175, 243);
  let flags_colour;
  if ($1 instanceof Ok) {
    flags_colour = $1[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "glint",
      644,
      "default_pretty_help",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $1,
        start: 18833,
        end: 18896,
        pattern_start: 18844,
        pattern_end: 18860
      }
    )
  }
  let $2 = $colour.from_rgb255(252, 226, 174);
  let subcommands_colour;
  if ($2 instanceof Ok) {
    subcommands_colour = $2[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "glint",
      645,
      "default_pretty_help",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $2,
        start: 18899,
        end: 18968,
        pattern_start: 18910,
        pattern_end: 18932
      }
    )
  }
  return new PrettyHelp(usage_colour, flags_colour, subcommands_colour);
}

/**
 * generate the string representation for the type of a flag
 * 
 * @ignore
 */
function flag_type_info(flag) {
  let $ = flag.value;
  if ($ instanceof B) {
    return "BOOL";
  } else if ($ instanceof I) {
    return "INT";
  } else if ($ instanceof LI) {
    return "INT_LIST";
  } else if ($ instanceof F) {
    return "FLOAT";
  } else if ($ instanceof LF) {
    return "FLOAT_LIST";
  } else if ($ instanceof S) {
    return "STRING";
  } else {
    return "STRING_LIST";
  }
}

/**
 * build the help representation for a list of subcommands
 * 
 * @ignore
 */
function build_subcommands_help(subcommands) {
  return $dict.fold(
    subcommands,
    toList([]),
    (acc, name, node) => {
      return listPrepend(new $help.Metadata(name, node.description), acc);
    },
  );
}

/**
 * initialize custom builders using a Value constructor and a parsing function
 * 
 * @ignore
 */
function new_builder(name, valuer, getter, p) {
  return new Flag(name, "", p, valuer, getter, new None());
}

/**
 * convert a (Flag(a) into its corresponding FlagEntry representation
 * 
 * @ignore
 */
function build_flag(fb) {
  return new FlagEntry(
    fb.value(new FlagInternals(fb.default, fb.parser)),
    fb.desc,
  );
}

function attempt(val, f) {
  return $result.try$(val, (a) => { return $result.replace(f(a), a); });
}

/**
 * attach a Constraint(a) to a Parser(a,Snag)
 * this function should not be used directly unless
 * 
 * @ignore
 */
function wrap_with_constraint(p, constraint) {
  return (input) => { return attempt(p(input), constraint); };
}

/**
 * Attach a constraint to a flag.
 *
 * As constraints are just functions, this works well as both part of a pipeline or with `use`.
 *
 *
 * ### Pipe:
 *
 * ```gleam
 * glint.int_flag("my_flag")
 * |> glint.flag_help("An awesome flag")
 * |> glint.flag_constraint(fn(i) {
 *   case i < 0 {
 *     True -> snag.error("must be greater than 0")
 *     False -> Ok(i)
 *   }})
 * ```
 *
 * ### Use:
 *
 * ```gleam
 * use i <- glint.flag_constraint(
 *   glint.int_flag("my_flag")
 *   |> glint.flag_help("An awesome flag")
 * )
 * case i < 0 {
 *   True -> snag.error("must be greater than 0")
 *   False -> Ok(i)
 * }
 * ```
 */
export function flag_constraint(builder, constraint) {
  return new Flag(
    builder.name,
    builder.desc,
    wrap_with_constraint(builder.parser, constraint),
    builder.value,
    builder.getter,
    builder.default,
  );
}

/**
 * Attach a help text description to a flag.
 *
 * This function allows for user-supplied newlines in long text strings. Individual newline characters are instead converted to spaces.
 * This is useful for developers to format their help text in a more readable way in the source code.
 *
 * For formatted text to appear on a new line, use 2 newline characters.
 * For formatted text to appear in a new paragraph, use 3 newline characters.
 *
 * ### Example:
 *
 * ```gleam
 * glint.int_flag("awesome_flag")
 * |> glint.flag_help("Some great text!")
 * ```
 */
export function flag_help(flag, description) {
  return new Flag(
    flag.name,
    description,
    flag.parser,
    flag.value,
    flag.getter,
    flag.default,
  );
}

/**
 * Set the default value for a flag.
 *
 * ### Example:
 *
 * ```gleam
 * glint.int_flag("awesome_flag")
 * |> glint.flag_default(1)
 * ```
 */
export function flag_default(flag, default$) {
  return new Flag(
    flag.name,
    flag.desc,
    flag.parser,
    flag.value,
    flag.getter,
    new Some(default$),
  );
}

function insert(flags, name, flag) {
  return new Flags($dict.insert(flags.internal, name, flag));
}

/**
 * Add a [`Flag(a)`](#Flag) to a [`Command(a)`](#Command)
 *
 * The provided callback is provided a function to fetch the current flag fvalue from the command input [`Flags`](#Flags).
 *
 * This function is most ergonomic as part of a `use` chain when building commands.
 *
 * ### Example:
 *
 * ```gleam
 * ...
 * use repeat <- glint.flag(
 *   glint.int_flag("repeat")
 *   |> glint.flag_default(1)
 *   |> glint.flag_help("Repeat the message n-times")
 * )
 * ...
 * use named, unnamed, flags <- glint.command()
 * let repeat_value = repeat(flags)
 * ```
 */
export function flag(flag, f) {
  let cmd = f((_capture) => { return flag.getter(_capture, flag.name); });
  return new Command(
    cmd.do,
    insert(cmd.flags, flag.name, build_flag(flag)),
    cmd.description,
    cmd.unnamed_args,
    cmd.named_args,
  );
}

function merge(a, b) {
  return new Flags($dict.merge(a.internal, b.internal));
}

function fold(flags, acc, f) {
  return $dict.fold(flags.internal, acc, f);
}

/**
 * build the help representation for a list of flags
 * 
 * @ignore
 */
function build_flags_help(flags) {
  return fold(
    flags,
    toList([]),
    (acc, name, flag) => {
      return listPrepend(
        new $help.Flag(
          new $help.Metadata(name, flag.description),
          flag_type_info(flag),
        ),
        acc,
      );
    },
  );
}

/**
 * build the help representation for a subtree of commands
 * 
 * @ignore
 */
function build_command_help(name, node) {
  let _block;
  let _pipe = node.contents;
  let _pipe$1 = $option.map(
    _pipe,
    (cmd) => {
      return [
        node.description,
        build_flags_help(merge(node.group_flags, cmd.flags)),
        cmd.unnamed_args,
        cmd.named_args,
      ];
    },
  );
  _block = $option.unwrap(
    _pipe$1,
    [node.description, toList([]), new None(), toList([])],
  );
  let $ = _block;
  let description;
  let flags;
  let unnamed_args$1;
  let named_args;
  description = $[0];
  flags = $[1];
  unnamed_args$1 = $[2];
  named_args = $[3];
  return new $help.Command(
    new $help.Metadata(name, description),
    flags,
    build_subcommands_help(node.subcommands),
    $option.map(
      unnamed_args$1,
      (args) => {
        if (args instanceof EqArgs) {
          let n = args[0];
          return new $help.EqArgs(n);
        } else {
          let n = args[0];
          return new $help.MinArgs(n);
        }
      },
    ),
    named_args,
  );
}

function new_flags() {
  return new Flags($dict.new$());
}

/**
 * Helper for initializing empty commands
 * 
 * @ignore
 */
function empty_command() {
  return new CommandNode(new None(), $dict.new$(), new_flags(), "");
}

/**
 * Create a [Command(a)](#Command) from a [Runner(a)](#Runner).
 *
 * ### Example:
 *
 * ```gleam
 * use <- glint.command_help("Some awesome help text")
 * use named_arg <- glint.named_arg("some_arg")
 * use <- glint.unnamed_args(glint.EqArgs(0))
 * ...
 * use named, unnamed, flags <- glint.command()
 * let my_arg = named_arg(named)
 * ...
 * ```
 */
export function command(runner) {
  return new Command(runner, new_flags(), "", new None(), toList([]));
}

function access_type_error(flag_type) {
  return $snag.error("cannot access flag as " + flag_type);
}

function flag_not_provided_error() {
  return $snag.error("no value provided");
}

function construct_value(input, internal, constructor) {
  return $result.map(
    internal.parser(input),
    (val) => {
      return constructor(new FlagInternals(new Some(val), internal.parser));
    },
  );
}

/**
 * Computes the new flag value given the input and the expected flag type
 * 
 * @ignore
 */
function compute_flag(input, current) {
  let _pipe = input;
  let _block;
  if (current instanceof B) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new B(var0); },
      );
    };
  } else if (current instanceof I) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new I(var0); },
      );
    };
  } else if (current instanceof LI) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new LI(var0); },
      );
    };
  } else if (current instanceof F) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new F(var0); },
      );
    };
  } else if (current instanceof LF) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new LF(var0); },
      );
    };
  } else if (current instanceof S) {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new S(var0); },
      );
    };
  } else {
    let internal = current[0];
    _block = (_capture) => {
      return construct_value(
        _capture,
        internal,
        (var0) => { return new LS(var0); },
      );
    };
  }
  let _pipe$1 = _block(_pipe);
  return $snag.context(_pipe$1, "failed to compute value for flag");
}

function layer_invalid_flag(err, flag) {
  return $snag.layer(err, ("invalid flag '" + flag) + "'");
}

function no_value_flag_err(flag_input) {
  let _pipe = (("flag '" + flag_input) + "' has no assigned value");
  let _pipe$1 = $snag.new$(_pipe);
  return layer_invalid_flag(_pipe$1, flag_input);
}

function undefined_flag_err(key) {
  let _pipe = "flag provided but not defined";
  let _pipe$1 = $snag.new$(_pipe);
  return layer_invalid_flag(_pipe$1, key);
}

function cannot_parse(value, kind) {
  let _pipe = ((("cannot parse value '" + value) + "' as ") + kind);
  return $snag.new$(_pipe);
}

/**
 * Access the contents for the associated flag
 * 
 * @ignore
 */
function get(flags, name) {
  let _pipe = $dict.get(flags.internal, name);
  return $result.replace_error(_pipe, undefined_flag_err(name));
}

function update_flag_value(flags, data) {
  let key;
  let input;
  key = data[0];
  input = data[1];
  return $result.try$(
    get(flags, key),
    (contents) => {
      return $result.map(
        (() => {
          let _pipe = compute_flag(input, contents.value);
          return $result.map_error(
            _pipe,
            (_capture) => { return layer_invalid_flag(_capture, key); },
          );
        })(),
        (value) => {
          return insert(flags, key, new FlagEntry(value, contents.description));
        },
      );
    },
  );
}

function attempt_toggle_flag(flags, key) {
  return $result.try$(
    get(flags, key),
    (contents) => {
      let $ = contents.value;
      if ($ instanceof B) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let internal = $[0];
          let val = $1[0];
          let _pipe = new FlagInternals(new Some(!val), internal.parser);
          let _pipe$1 = new B(_pipe);
          let _pipe$2 = ((val) => {
            return new FlagEntry(val, contents.description);
          })(_pipe$1);
          let _pipe$3 = $dict.insert(flags.internal, key, _pipe$2);
          let _pipe$4 = new Flags(_pipe$3);
          return new Ok(_pipe$4);
        } else {
          let internal = $[0];
          let _pipe = new FlagInternals(new Some(true), internal.parser);
          let _pipe$1 = new B(_pipe);
          let _pipe$2 = ((val) => {
            return new FlagEntry(val, contents.description);
          })(_pipe$1);
          let _pipe$3 = $dict.insert(flags.internal, key, _pipe$2);
          let _pipe$4 = new Flags(_pipe$3);
          return new Ok(_pipe$4);
        }
      } else {
        return new Error(no_value_flag_err(key));
      }
    },
  );
}

function get_value(flags, key, kind) {
  let _pipe = get(flags, key);
  let _pipe$1 = $result.try$(_pipe, kind);
  return $snag.context(
    _pipe$1,
    ("failed to retrieve value for flag '" + key) + "'",
  );
}

/**
 * Gets the value for the associated flag.
 *
 * This function should only ever be used when fetching flags set at the group level.
 * For local flags please use the getter functions provided when calling [`glint.flag`](#flag).
 */
export function get_flag(flags, flag) {
  return flag.getter(flags, flag.name);
}

/**
 * Gets the current value for the associated int flag
 * 
 * @ignore
 */
function get_int_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof I) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let val = $1[0];
          return new Ok(val);
        } else {
          return flag_not_provided_error();
        }
      } else {
        return access_type_error("int");
      }
    },
  );
}

/**
 * Initialise an int flag.
 */
export function int_flag(name) {
  return new_builder(
    name,
    (var0) => { return new I(var0); },
    get_int_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $int.parse(_pipe);
      return $result.replace_error(_pipe$1, cannot_parse(input, "int"));
    },
  );
}

/**
 * Gets the current value for the associated ints flag
 * 
 * @ignore
 */
function get_ints_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof LI) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let val = $1[0];
          return new Ok(val);
        } else {
          return flag_not_provided_error();
        }
      } else {
        return access_type_error("int list");
      }
    },
  );
}

/**
 * Initialise an int list flag.
 */
export function ints_flag(name) {
  return new_builder(
    name,
    (var0) => { return new LI(var0); },
    get_ints_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $string.split(_pipe, ",");
      let _pipe$2 = $list.try_map(_pipe$1, $int.parse);
      return $result.replace_error(_pipe$2, cannot_parse(input, "int list"));
    },
  );
}

/**
 * Gets the current value for the associated bool flag
 * 
 * @ignore
 */
function get_bool_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof B) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let val = $1[0];
          return new Ok(val);
        } else {
          return flag_not_provided_error();
        }
      } else {
        return access_type_error("bool");
      }
    },
  );
}

/**
 * Initialise a boolean flag.
 */
export function bool_flag(name) {
  return new_builder(
    name,
    (var0) => { return new B(var0); },
    get_bool_flag,
    (input) => {
      let $ = $string.lowercase(input);
      if ($ === "true") {
        return new Ok(true);
      } else if ($ === "t") {
        return new Ok(true);
      } else if ($ === "false") {
        return new Ok(false);
      } else if ($ === "f") {
        return new Ok(false);
      } else {
        return new Error(cannot_parse(input, "bool"));
      }
    },
  );
}

/**
 * Gets the current value for the associated string flag
 * 
 * @ignore
 */
function get_string_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof S) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let val = $1[0];
          return new Ok(val);
        } else {
          return flag_not_provided_error();
        }
      } else {
        return access_type_error("string");
      }
    },
  );
}

/**
 * Initialise a string flag.
 */
export function string_flag(name) {
  return new_builder(
    name,
    (var0) => { return new S(var0); },
    get_string_flag,
    (s) => { return new Ok(s); },
  );
}

/**
 * Gets the current value for the associated strings flag
 * 
 * @ignore
 */
function get_strings_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof LS) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let val = $1[0];
          return new Ok(val);
        } else {
          return flag_not_provided_error();
        }
      } else {
        return access_type_error("string list");
      }
    },
  );
}

/**
 * Intitialise a string list flag.
 */
export function strings_flag(name) {
  return new_builder(
    name,
    (var0) => { return new LS(var0); },
    get_strings_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $string.split(_pipe, ",");
      return new Ok(_pipe$1);
    },
  );
}

/**
 * Gets the current value for the associated float flag
 * 
 * @ignore
 */
function get_floats(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof F) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let val = $1[0];
          return new Ok(val);
        } else {
          return flag_not_provided_error();
        }
      } else {
        return access_type_error("float");
      }
    },
  );
}

/**
 *Initialise a float flag.
 */
export function float_flag(name) {
  return new_builder(
    name,
    (var0) => { return new F(var0); },
    get_floats,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $float.parse(_pipe);
      return $result.replace_error(_pipe$1, cannot_parse(input, "float"));
    },
  );
}

/**
 * Gets the current value for the associated float flag
 * 
 * @ignore
 */
function get_floats_flag(flags, name) {
  return get_value(
    flags,
    name,
    (flag) => {
      let $ = flag.value;
      if ($ instanceof LF) {
        let $1 = $[0].value;
        if ($1 instanceof Some) {
          let val = $1[0];
          return new Ok(val);
        } else {
          return flag_not_provided_error();
        }
      } else {
        return access_type_error("float list");
      }
    },
  );
}

/**
 * Initialise a float list flag.
 */
export function floats_flag(name) {
  return new_builder(
    name,
    (var0) => { return new LF(var0); },
    get_floats_flag,
    (input) => {
      let _pipe = input;
      let _pipe$1 = $string.split(_pipe, ",");
      let _pipe$2 = $list.try_map(_pipe$1, $float.parse);
      return $result.replace_error(_pipe$2, cannot_parse(input, "float list"));
    },
  );
}

function do_update_at(node, path, f) {
  if (path instanceof $Empty) {
    return f(node);
  } else {
    let next = path.head;
    let rest = path.tail;
    return new CommandNode(
      node.contents,
      $dict.upsert(
        node.subcommands,
        next,
        (found) => {
          let _pipe = found;
          let _pipe$1 = $option.lazy_unwrap(_pipe, empty_command);
          return do_update_at(_pipe$1, rest, f);
        },
      ),
      node.group_flags,
      node.description,
    );
  }
}

function update_at(glint, path, f) {
  return new Glint(
    glint.config,
    do_update_at(glint.cmd, sanitize_path(path), f),
  );
}

/**
 * Set the help text for a specific command path.
 *
 * This function is intended to allow users to set the help text of commands that might not be directly instantiated,
 * such as commands with no business logic associated to them but that have subcommands.
 *
 * Using this function should almost never be necessary, in most cases you should use [`glint.command_help`](#command_help) insstead.
 */
export function path_help(glint, path, description) {
  return update_at(
    glint,
    path,
    (node) => {
      return new CommandNode(
        node.contents,
        node.subcommands,
        node.group_flags,
        description,
      );
    },
  );
}

/**
 * Adds a new command to be run at the specified path.
 *
 * If the path is `[]`, the root command is set with the provided function and
 * flags.
 *
 * Note: all command paths are sanitized by stripping whitespace and removing any empty string elements.
 *
 * ```gleam
 * glint.new()
 * |> glint.add(at: [], do: root_command())
 * |> glint.add(at: ["subcommand"], do: subcommand())
 * ...
 * ```
 */
export function add(glint, path, command) {
  return update_at(
    glint,
    path,
    (node) => {
      return new CommandNode(
        new Some(
          new InternalCommand(
            command.do,
            command.flags,
            command.unnamed_args,
            command.named_args,
          ),
        ),
        node.subcommands,
        node.group_flags,
        command.description,
      );
    },
  );
}

/**
 * Add a flag for a group of commands.
 * The provided flags will be available to all commands at or beyond the provided path
 */
export function group_flag(glint, path, flag) {
  return update_at(
    glint,
    path,
    (node) => {
      return new CommandNode(
        node.contents,
        node.subcommands,
        insert(node.group_flags, flag.name, build_flag(flag)),
        node.description,
      );
    },
  );
}

/**
 * default config
 * 
 * @ignore
 */
const default_config = /* @__PURE__ */ new Config(
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  false,
  /* @__PURE__ */ new None(),
  true,
  4,
  80,
  20,
  2,
);

/**
 * Create a new glint instance.
 */
export function new$() {
  return new Glint(default_config, empty_command());
}

/**
 * FlagEntry inputs must start with this prefix
 * 
 * @ignore
 */
const flag_prefix = "--";

/**
 * The separation character for flag names and their values
 * 
 * @ignore
 */
const flag_delimiter = "=";

function build_help_config(config) {
  return new $help.Config(
    config.name,
    $option.map(config.pretty_help, (p) => { return p.usage; }),
    $option.map(config.pretty_help, (p) => { return p.flags; }),
    $option.map(config.pretty_help, (p) => { return p.subcommands; }),
    config.as_module,
    config.description,
    config.indent_width,
    config.max_output_width,
    config.min_first_column_width,
    config.column_gap,
    flag_prefix,
    flag_delimiter,
  );
}

/**
 * generate the help text for a command
 * 
 * @ignore
 */
function cmd_help(path, cmd, config) {
  let _pipe = path;
  let _pipe$1 = $list.reverse(_pipe);
  let _pipe$2 = $string.join(_pipe$1, " ");
  let _pipe$3 = build_command_help(_pipe$2, cmd);
  return $help.command_help_to_string(_pipe$3, build_help_config(config));
}

/**
 * Updates a flag value, ensuring that the new value can satisfy the required type.
 * Assumes that all flag inputs passed in start with --
 * This function is only intended to be used from glint.execute_root
 * 
 * @ignore
 */
function update_flags(flags, flag_input) {
  let flag_input$1 = $string.drop_start(flag_input, $string.length(flag_prefix));
  let $ = $string.split_once(flag_input$1, flag_delimiter);
  if ($ instanceof Ok) {
    let data = $[0];
    return update_flag_value(flags, data);
  } else {
    return attempt_toggle_flag(flags, flag_input$1);
  }
}

/**
 * Executes the current root command.
 * 
 * @ignore
 */
function execute_root(path, config, cmd, args, flag_inputs) {
  let _pipe = $result.try$(
    $option.to_result(cmd.contents, $snag.new$("command not found")),
    (contents) => {
      return $result.try$(
        $list.try_fold(
          flag_inputs,
          merge(cmd.group_flags, contents.flags),
          update_flags,
        ),
        (new_flags) => {
          return $result.try$(
            (() => {
              let named = $list.zip(contents.named_args, args);
              let $ = $list.length(named) === $list.length(contents.named_args);
              if ($) {
                return new Ok($dict.from_list(named));
              } else {
                return $snag.error(
                  "unmatched named arguments: " + (() => {
                    let _pipe = contents.named_args;
                    let _pipe$1 = $list.drop(_pipe, $list.length(named));
                    let _pipe$2 = $list.map(
                      _pipe$1,
                      (s) => { return ("'" + s) + "'"; },
                    );
                    return $string.join(_pipe$2, ", ");
                  })(),
                );
              }
            })(),
            (named_args) => {
              let args$1 = $list.drop(args, $dict.size(named_args));
              return $result.map(
                (() => {
                  let $ = contents.unnamed_args;
                  if ($ instanceof Some) {
                    let count = $[0];
                    let _pipe = count;
                    let _pipe$1 = args_compare(_pipe, $list.length(args$1));
                    return $snag.context(
                      _pipe$1,
                      "invalid number of arguments provided",
                    );
                  } else {
                    return new Ok(undefined);
                  }
                })(),
                (_) => {
                  return contents.do(
                    new NamedArgs(named_args),
                    args$1,
                    new_flags,
                  );
                },
              );
            },
          );
        },
      );
    },
  );
  return $result.map_error(
    _pipe,
    (err) => {
      return ((() => {
        let _pipe$1 = err;
        let _pipe$2 = $snag.layer(_pipe$1, "failed to run command");
        return $snag.pretty_print(_pipe$2);
      })() + "\nSee the following help text, available via the '--help' flag.\n\n") + cmd_help(
        path,
        cmd,
        config,
      );
    },
  );
}

/**
 * Find which command to execute and run it with computed flags and args
 * 
 * @ignore
 */
function do_execute(
  loop$cmd,
  loop$config,
  loop$args,
  loop$flags,
  loop$help,
  loop$command_path
) {
  while (true) {
    let cmd = loop$cmd;
    let config = loop$config;
    let args = loop$args;
    let flags = loop$flags;
    let help = loop$help;
    let command_path = loop$command_path;
    if (args instanceof $Empty) {
      if (help) {
        return new Ok(new Help(cmd_help(command_path, cmd, config)));
      } else {
        let _pipe = execute_root(command_path, config, cmd, toList([]), flags);
        return $result.map(_pipe, (var0) => { return new Out(var0); });
      }
    } else {
      let arg = args.head;
      let rest = args.tail;
      let $ = $dict.get(cmd.subcommands, arg);
      if ($ instanceof Ok) {
        let sub_command = $[0];
        let _pipe = new CommandNode(
          sub_command.contents,
          sub_command.subcommands,
          merge(cmd.group_flags, sub_command.group_flags),
          sub_command.description,
        );
        loop$cmd = _pipe;
        loop$config = config;
        loop$args = rest;
        loop$flags = flags;
        loop$help = help;
        loop$command_path = listPrepend(arg, command_path);
      } else if (help) {
        return new Ok(new Help(cmd_help(command_path, cmd, config)));
      } else {
        let _pipe = execute_root(command_path, config, cmd, args, flags);
        return $result.map(_pipe, (var0) => { return new Out(var0); });
      }
    }
  }
}

/**
 * Determines which command to run and executes it.
 *
 * Sets any provided flags if necessary.
 *
 * Each value prefixed with `--` is parsed as a flag.
 *
 * This function does not print its output and is mainly intended for use within `glint` itself.
 * If you would like to print or handle the output of a command please see the `run_and_handle` function.
 * 
 * @ignore
 */
export function execute(glint, args) {
  let help_flag = flag_prefix + $help.help_flag.meta.name;
  let _block;
  let $1 = $list.partition(args, (s) => { return s === help_flag; });
  let $2 = $1[0];
  if ($2 instanceof $Empty) {
    let args$1 = $1[1];
    _block = [false, args$1];
  } else {
    let args$1 = $1[1];
    _block = [true, args$1];
  }
  let $ = _block;
  let help;
  let args$1;
  help = $[0];
  args$1 = $[1];
  let $3 = $list.partition(
    args$1,
    (_capture) => { return $string.starts_with(_capture, flag_prefix); },
  );
  let flags;
  let args$2;
  flags = $3[0];
  args$2 = $3[1];
  return do_execute(glint.cmd, glint.config, args$2, flags, help, toList([]));
}

/**
 * Run a glint app with a custom handler for command output.
 * This function prints any errors enountered or the help text if requested.
 *
 * IMPORTANT: This function exits with code 1 if an error was encountered.
 * If this behaviour is not desired please disable it with [`glint.without_exit`](#without_exit)
 */
export function run_and_handle(glint, args, handle) {
  let $ = execute(glint, args);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof Out) {
      let out = $1[0];
      handle(out);
      return undefined;
    } else {
      let s = $1[0];
      return $io.println(s);
    }
  } else {
    let s = $[0];
    $io.println(s);
    let $1 = glint.config.exit;
    if ($1) {
      return exit(1);
    } else {
      return undefined;
    }
  }
}

/**
 * Run a glint app and print any errors enountered, or the help text if requested.
 * This function ignores any value returned by the command that was run.
 * If you would like to do handle the command output please see the [`glint.run_and_handle`](#run_and_handle) function.
 *
 * IMPORTANT: This function exits with code 1 if an error was encountered.
 * If this behaviour is not desired please disable it with [`glint.without_exit`](#without_exit)
 */
export function run(glint, args) {
  return run_and_handle(glint, args, (_) => { return undefined; });
}
