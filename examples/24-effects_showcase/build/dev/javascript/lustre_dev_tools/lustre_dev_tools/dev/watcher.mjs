import * as $booklet from "../../../booklet/booklet.mjs";
import * as $filepath from "../../../filepath/filepath.mjs";
import * as $ansi from "../../../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import { Started } from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $group_registry from "../../../group_registry/group_registry.mjs";
import * as $justin from "../../../justin/justin.mjs";
import * as $polly from "../../../polly/polly.mjs";
import * as $simplifile from "../../../simplifile/simplifile.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $bun from "../../lustre_dev_tools/bin/bun.mjs";
import * as $gleam from "../../lustre_dev_tools/bin/gleam.mjs";
import * as $tailwind from "../../lustre_dev_tools/bin/tailwind.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";
import * as $system from "../../lustre_dev_tools/system.mjs";

export class Change extends $CustomType {
  constructor(in$, path) {
    super();
    this.in = in$;
    this.path = path;
  }
}

export class Styles extends $CustomType {}

export class BuildError extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
