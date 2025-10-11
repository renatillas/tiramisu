import * as $booklet from "../../../booklet/booklet.mjs";
import * as $filepath from "../../../filepath/filepath.mjs";
import * as $application from "../../../gleam_erlang/gleam/erlang/application.mjs";
import * as $request from "../../../gleam_http/gleam/http/request.mjs";
import * as $actor from "../../../gleam_otp/gleam/otp/actor.mjs";
import * as $static_supervisor from "../../../gleam_otp/gleam/otp/static_supervisor.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $mist from "../../../mist/mist.mjs";
import * as $wisp from "../../../wisp/wisp.mjs";
import * as $wisp_mist from "../../../wisp/wisp/wisp_mist.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $html from "../../lustre_dev_tools/build/html.mjs";
import * as $cli from "../../lustre_dev_tools/cli.mjs";
import * as $live_reload from "../../lustre_dev_tools/dev/live_reload.mjs";
import * as $proxy from "../../lustre_dev_tools/dev/proxy.mjs";
import * as $watcher from "../../lustre_dev_tools/dev/watcher.mjs";
import * as $error from "../../lustre_dev_tools/error.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";

class Context extends $CustomType {
  constructor(project, entry, tailwind_entry, priv, proxy) {
    super();
    this.project = project;
    this.entry = entry;
    this.tailwind_entry = tailwind_entry;
    this.priv = priv;
    this.proxy = proxy;
  }
}
