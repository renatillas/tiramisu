import * as $argv from "../../argv/argv.mjs";
import * as $booklet from "../../booklet/booklet.mjs";
import * as $filepath from "../../filepath/filepath.mjs";
import * as $process from "../../gleam_erlang/gleam/erlang/process.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $glint from "../../glint/glint.mjs";
import * as $justin from "../../justin/justin.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $bun from "../lustre_dev_tools/bin/bun.mjs";
import * as $gleam from "../lustre_dev_tools/bin/gleam.mjs";
import * as $tailwind from "../lustre_dev_tools/bin/tailwind.mjs";
import * as $html from "../lustre_dev_tools/build/html.mjs";
import * as $cli from "../lustre_dev_tools/cli.mjs";
import * as $proxy from "../lustre_dev_tools/dev/proxy.mjs";
import * as $server from "../lustre_dev_tools/dev/server.mjs";
import * as $watcher from "../lustre_dev_tools/dev/watcher.mjs";
import * as $error from "../lustre_dev_tools/error.mjs";
import * as $project from "../lustre_dev_tools/project.mjs";
import * as $system from "../lustre_dev_tools/system.mjs";

class AddOptions extends $CustomType {
  constructor(integrations) {
    super();
    this.integrations = integrations;
  }
}

class BuildOptions extends $CustomType {
  constructor(minify, outdir, entries, skip_html, skip_tailwind) {
    super();
    this.minify = minify;
    this.outdir = outdir;
    this.entries = entries;
    this.skip_html = skip_html;
    this.skip_tailwind = skip_tailwind;
  }
}

class StartOptions extends $CustomType {
  constructor(watch, proxy, entry, tailwind_entry, host, port) {
    super();
    this.watch = watch;
    this.proxy = proxy;
    this.entry = entry;
    this.tailwind_entry = tailwind_entry;
    this.host = host;
    this.port = port;
  }
}
