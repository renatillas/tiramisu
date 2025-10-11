import * as $filepath from "../filepath/filepath.mjs";
import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../gleam_stdlib/gleam/option.mjs";
import * as $order from "../gleam_stdlib/gleam/order.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import { Eacces, Enoent } from "../simplifile/simplifile.mjs";
import * as $gleam from "./gleam.mjs";
import {
  Error as Err,
  Ok,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "./gleam.mjs";
import { repeatedly } from "./polly_ffi.mjs";

/**
 * A new file or dictionary was created!
 */
export class Created extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}

/**
 * A file got modified!
 */
export class Changed extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}

/**
 * A file or dictionary got deleted :(
 */
export class Deleted extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}

/**
 * An unexpected simplifile error happened :(
 * 
 * Polly treats `Enoent` and `Eacces` errors as if the file got deleted,
 * and will therefore never pass those to you.
 */
export class Error extends $CustomType {
  constructor(path, reason) {
    super();
    this.path = path;
    this.reason = reason;
  }
}

class Options extends $CustomType {
  constructor(interval, paths, max_depth, filter, ignore_initial_missing) {
    super();
    this.interval = interval;
    this.paths = paths;
    this.max_depth = max_depth;
    this.filter = filter;
    this.ignore_initial_missing = ignore_initial_missing;
  }
}

class Watcher extends $CustomType {
  constructor(stop) {
    super();
    this.stop = stop;
  }
}

class File extends $CustomType {
  constructor(name, modkey) {
    super();
    this.name = name;
    this.modkey = modkey;
  }
}

class Folder extends $CustomType {
  constructor(name, modkey, children) {
    super();
    this.name = name;
    this.modkey = modkey;
    this.children = children;
  }
}

/**
 * Tell Polly which directory to watch. If it does not exist, `watch` will return an error.
 * 
 * If the directory goes away after watching has started, Polly will continue to
 * check on it to see if it came back.
 * 
 * Paths are not expanded by default, so the paths reported by events and passed
 * to the filter function will be prefixed with whatever you specified here.
 */
export function add_dir(options, path) {
  let interval$1;
  let paths;
  let max_depth$1;
  let filter$1;
  let ignore_initial_missing$1;
  interval$1 = options.interval;
  paths = options.paths;
  max_depth$1 = options.max_depth;
  filter$1 = options.filter;
  ignore_initial_missing$1 = options.ignore_initial_missing;
  return new Options(
    interval$1,
    listPrepend(path, paths),
    max_depth$1,
    filter$1,
    ignore_initial_missing$1,
  );
}

/**
 * Tell Polly to watch a single file.
 * 
 * Polly doesn't care if you tell her to watch a file or directory, but
 * using this function makes your intent clearer!
 */
export function add_file(options, path) {
  return add_dir(options, path);
}

/**
 * Limit the maximum depth that Polly will walk each directory.
 *
 * A limit of `0` would mean that Polly _only_ watches the specified list of
 * files or directories. A limit of `1` means that she will also look at the
 * files inside the given directories, but not at any nested directories.
 * 
 * There is no limit by default, but setting a limit might be good to
 * better control resource usage of the watcher.
 * 
 * Calling this function multiple times will cause polly to only remember the
 * lowest limit provided.
 */
export function max_depth(options, max_depth) {
  let $ = (options.max_depth < 0) || (max_depth < options.max_depth);
  if ($) {
    return new Options(
      options.interval,
      options.paths,
      max_depth,
      options.filter,
      options.ignore_initial_missing,
    );
  } else {
    return options;
  }
}

/**
 * Set the interval in-between file-system polls, in milliseconds.
 * 
 * This is the time that Polly rests between calls, so if scanning your directory
 * tree takes 100ms, and you configured 1000ms here, the total time between calls
 * will roughly be 1100ms.
 * 
 * Doing it this way makes sure that Polly doesn't stumble over herself.
 */
export function interval(options, interval) {
  let $ = interval > 0;
  if ($) {
    return new Options(
      interval,
      options.paths,
      options.max_depth,
      options.filter,
      options.ignore_initial_missing,
    );
  } else {
    return options;
  }
}

/**
 * Filter files using the given predicate.
 * 
 * Polly will ignore files and directories for which the predicate returns `False`
 * completely, and any event happening for them or for a contained file of them
 * will not get reported.
 * 
 * Keep in mind that the filter is checked for every part of a path, not just
 * leaf nodes! So for example, if you have a path `./src/app.gleam`, your filter
 * function will first be called on `.`, then on `./src`, and then finally on
 * `./src/app.gleam`.
 * 
 * By default, all hidden files are ignored by using the `default_filter`.
 */
export function filter(options, filter) {
  return new Options(
    options.interval,
    options.paths,
    options.max_depth,
    filter,
    options.ignore_initial_missing,
  );
}

/**
 * The default filter function, ignoring hidden files starting with a colon `"."`
 */
export function default_filter(_, path) {
  let $ = $filepath.base_name(path);
  if ($ === ".") {
    return true;
  } else if ($ === "..") {
    return true;
  } else {
    let basename = $;
    return !$string.starts_with(basename, ".");
  }
}

/**
 * Start creating a new configuration using the default options.
 * 
 * By default, an interval of 1 second is set, and the `default_filter` is used.
 */
export function new$() {
  return new Options(1000, toList([]), -1, default_filter, false);
}

/**
 * Tell Polly that it is fine if a file or directory does not exist initially.
 * 
 * By default, if a file or directory cannot be found when calling `watch`,
 * Polly will immediately return to you with an `Enoent` error and refuse to run.
 * 
 * When this option is active, Polly will instead note the missing directory,
 * and continuously check if it appears, similarly to how she does after a
 * file or directory goes away after she has first seen it.
 */
export function ignore_initial_missing(options) {
  return new Options(
    options.interval,
    options.paths,
    options.max_depth,
    options.filter,
    true,
  );
}

function collect_errors(errors, event) {
  if (event instanceof Error) {
    let path = event.path;
    let reason = event.reason;
    return listPrepend([path, reason], errors);
  } else {
    return errors;
  }
}

/**
 * Stop this watcher.
 * 
 * If Polly currently scans your directories, she might not hear you right away
 * and may still report events for one run, after which she will stop.
 */
export function stop(watcher) {
  return watcher.stop();
}

function delete$(path, vfs, state, emit) {
  let full_path = $filepath.join(path, vfs.name);
  if (vfs instanceof File) {
    return emit(state, new Deleted(full_path));
  } else {
    let children = vfs.children;
    let state$1 = $list.fold(
      children,
      state,
      (state, child) => { return delete$(full_path, child, state, emit); },
    );
    return emit(state$1, new Deleted(full_path));
  }
}

function get_modkey(stat) {
  return $int.max(stat.mtime_seconds, stat.ctime_seconds);
}

function readdir(path) {
  let _pipe = $simplifile.read_directory(path);
  return $result.map(
    _pipe,
    (_capture) => { return $list.sort(_capture, $string.compare); },
  );
}

function diff_children(
  loop$filter,
  loop$depth,
  loop$path,
  loop$old_children,
  loop$new_entries,
  loop$new_children,
  loop$state,
  loop$emit
) {
  while (true) {
    let filter = loop$filter;
    let depth = loop$depth;
    let path = loop$path;
    let old_children = loop$old_children;
    let new_entries = loop$new_entries;
    let new_children = loop$new_children;
    let state = loop$state;
    let emit = loop$emit;
    if (new_entries instanceof $Empty) {
      if (old_children instanceof $Empty) {
        return [$list.reverse(new_children), state];
      } else {
        let first_old = old_children.head;
        let rest_old = old_children.tail;
        loop$filter = filter;
        loop$depth = depth;
        loop$path = path;
        loop$old_children = rest_old;
        loop$new_entries = new_entries;
        loop$new_children = new_children;
        loop$state = delete$(path, first_old, state, emit);
        loop$emit = emit;
      }
    } else if (old_children instanceof $Empty) {
      let first_new = new_entries.head;
      let rest_new = new_entries.tail;
      let $ = create(filter, depth, path, first_new, state, emit);
      let new_vfs;
      let state$1;
      new_vfs = $[0];
      state$1 = $[1];
      let _block;
      if (new_vfs instanceof Some) {
        let new_vfs$1 = new_vfs[0];
        _block = listPrepend(new_vfs$1, new_children);
      } else {
        _block = new_children;
      }
      let new_children$1 = _block;
      loop$filter = filter;
      loop$depth = depth;
      loop$path = path;
      loop$old_children = old_children;
      loop$new_entries = rest_new;
      loop$new_children = new_children$1;
      loop$state = state$1;
      loop$emit = emit;
    } else {
      let first_new = new_entries.head;
      let rest_new = new_entries.tail;
      let first_old = old_children.head;
      let rest_old = old_children.tail;
      let $ = $string.compare(first_old.name, first_new);
      if ($ instanceof $order.Lt) {
        loop$filter = filter;
        loop$depth = depth;
        loop$path = path;
        loop$old_children = rest_old;
        loop$new_entries = new_entries;
        loop$new_children = new_children;
        loop$state = delete$(path, first_old, state, emit);
        loop$emit = emit;
      } else if ($ instanceof $order.Eq) {
        let $1 = diff(filter, depth, path, first_old, state, emit);
        let $2 = $1[0];
        if ($2 instanceof Some) {
          let state$1 = $1[1];
          let new_vfs = $2[0];
          loop$filter = filter;
          loop$depth = depth;
          loop$path = path;
          loop$old_children = rest_old;
          loop$new_entries = rest_new;
          loop$new_children = listPrepend(new_vfs, new_children);
          loop$state = state$1;
          loop$emit = emit;
        } else {
          let state$1 = $1[1];
          loop$filter = filter;
          loop$depth = depth;
          loop$path = path;
          loop$old_children = rest_old;
          loop$new_entries = rest_new;
          loop$new_children = new_children;
          loop$state = state$1;
          loop$emit = emit;
        }
      } else {
        let $1 = create(filter, depth, path, first_new, state, emit);
        let new_vfs;
        let state$1;
        new_vfs = $1[0];
        state$1 = $1[1];
        let _block;
        if (new_vfs instanceof Some) {
          let new_vfs$1 = new_vfs[0];
          _block = listPrepend(new_vfs$1, new_children);
        } else {
          _block = new_children;
        }
        let new_children$1 = _block;
        loop$filter = filter;
        loop$depth = depth;
        loop$path = path;
        loop$old_children = old_children;
        loop$new_entries = rest_new;
        loop$new_children = new_children$1;
        loop$state = state$1;
        loop$emit = emit;
      }
    }
  }
}

function diff(filter, depth, path, vfs, state, emit) {
  let full_path = $filepath.join(path, vfs.name);
  let $ = $simplifile.link_info(full_path);
  if ($ instanceof Ok) {
    let stat = $[0];
    let type_ = $simplifile.file_info_type(stat);
    let $1 = filter(type_, full_path);
    if ($1) {
      if (vfs instanceof File) {
        if (type_ instanceof $simplifile.File) {
          let name = vfs.name;
          let old_key = vfs.modkey;
          let new_key = get_modkey(stat);
          let $2 = new_key === old_key;
          if ($2) {
            return [new Some(vfs), state];
          } else {
            return [
              new Some(new File(name, new_key)),
              emit(state, new Changed(full_path)),
            ];
          }
        } else {
          return create_stat(
            filter,
            depth,
            vfs.name,
            full_path,
            stat,
            delete$(path, vfs, state, emit),
            emit,
          );
        }
      } else if (type_ instanceof $simplifile.Directory) {
        if (depth === 0) {
          return [new Some(vfs), state];
        } else if (depth !== 0) {
          let name = vfs.name;
          let old_children = vfs.children;
          let $2 = readdir(full_path);
          if ($2 instanceof Ok) {
            let new_entries = $2[0];
            let $3 = diff_children(
              filter,
              depth - 1,
              full_path,
              old_children,
              new_entries,
              toList([]),
              state,
              emit,
            );
            let children;
            let state$1;
            children = $3[0];
            state$1 = $3[1];
            return [
              new Some(new Folder(name, get_modkey(stat), children)),
              state$1,
            ];
          } else {
            let $3 = $2[0];
            if ($3 instanceof Eacces) {
              return [new None(), delete$(path, vfs, state, emit)];
            } else if ($3 instanceof Enoent) {
              return [new None(), delete$(path, vfs, state, emit)];
            } else {
              let reason = $3;
              return [new Some(vfs), emit(state, new Error(full_path, reason))];
            }
          }
        } else {
          return create_stat(
            filter,
            depth,
            vfs.name,
            full_path,
            stat,
            delete$(path, vfs, state, emit),
            emit,
          );
        }
      } else {
        return create_stat(
          filter,
          depth,
          vfs.name,
          full_path,
          stat,
          delete$(path, vfs, state, emit),
          emit,
        );
      }
    } else {
      return [new None(), delete$(path, vfs, state, emit)];
    }
  } else {
    let $1 = $[0];
    if ($1 instanceof Eacces) {
      return [new None(), delete$(path, vfs, state, emit)];
    } else if ($1 instanceof Enoent) {
      return [new None(), delete$(path, vfs, state, emit)];
    } else {
      let reason = $1;
      return [new Some(vfs), emit(state, new Error(path, reason))];
    }
  }
}

function create_children(
  loop$filter,
  loop$depth,
  loop$path,
  loop$children,
  loop$oks,
  loop$state,
  loop$emit
) {
  while (true) {
    let filter = loop$filter;
    let depth = loop$depth;
    let path = loop$path;
    let children = loop$children;
    let oks = loop$oks;
    let state = loop$state;
    let emit = loop$emit;
    if (children instanceof $Empty) {
      return [$list.reverse(oks), state];
    } else {
      let first = children.head;
      let rest = children.tail;
      let $ = create(filter, depth, path, first, state, emit);
      let $1 = $[0];
      if ($1 instanceof Some) {
        let state$1 = $[1];
        let vfs = $1[0];
        loop$filter = filter;
        loop$depth = depth;
        loop$path = path;
        loop$children = rest;
        loop$oks = listPrepend(vfs, oks);
        loop$state = state$1;
        loop$emit = emit;
      } else {
        let state$1 = $[1];
        loop$filter = filter;
        loop$depth = depth;
        loop$path = path;
        loop$children = rest;
        loop$oks = oks;
        loop$state = state$1;
        loop$emit = emit;
      }
    }
  }
}

function create(filter, depth, path, name, state, emit) {
  let full_path = $filepath.join(path, name);
  let $ = $simplifile.link_info(full_path);
  if ($ instanceof Ok) {
    let stat = $[0];
    return create_stat(filter, depth, name, full_path, stat, state, emit);
  } else {
    let $1 = $[0];
    if ($1 instanceof Eacces) {
      return [new None(), state];
    } else if ($1 instanceof Enoent) {
      return [new None(), state];
    } else {
      let reason = $1;
      return [new None(), emit(state, new Error(full_path, reason))];
    }
  }
}

function create_stat(filter, depth, name, full_path, stat, state, emit) {
  let type_ = $simplifile.file_info_type(stat);
  return $bool.guard(
    !filter(type_, full_path),
    [new None(), state],
    () => {
      if (type_ instanceof $simplifile.File) {
        return [
          new Some(new File(name, get_modkey(stat))),
          emit(state, new Created(full_path)),
        ];
      } else if (type_ instanceof $simplifile.Directory) {
        if (depth === 0) {
          return [
            new Some(new Folder(name, get_modkey(stat), toList([]))),
            emit(state, new Created(full_path)),
          ];
        } else if (depth !== 0) {
          let $ = readdir(full_path);
          if ($ instanceof Ok) {
            let entries = $[0];
            let depth$1 = depth - 1;
            let state$1 = emit(state, new Created(full_path));
            let $1 = create_children(
              filter,
              depth$1,
              full_path,
              entries,
              toList([]),
              state$1,
              emit,
            );
            let children;
            let state$2;
            children = $1[0];
            state$2 = $1[1];
            return [
              new Some(new Folder(name, get_modkey(stat), children)),
              state$2,
            ];
          } else {
            let $1 = $[0];
            if ($1 instanceof Eacces) {
              return [new None(), state];
            } else if ($1 instanceof Enoent) {
              return [new None(), state];
            } else {
              let reason = $1;
              return [new None(), emit(state, new Error(full_path, reason))];
            }
          }
        } else {
          return [new None(), state];
        }
      } else {
        return [new None(), state];
      }
    },
  );
}

/**
 * /// Like `watch`, but similar to `list.fold` Polly will also keep some state
 * around for you and pass it back on each invocation.
 */
export function watch_with(options, initial, emit) {
  let interval$1;
  let paths;
  let max_depth$1;
  let filter$1;
  let ignore_initial_missing$1;
  interval$1 = options.interval;
  paths = options.paths;
  max_depth$1 = options.max_depth;
  filter$1 = options.filter;
  ignore_initial_missing$1 = options.ignore_initial_missing;
  return $result.try$(
    $list.try_map(
      paths,
      (path) => {
        let $ = create(
          filter$1,
          max_depth$1,
          path,
          "",
          toList([]),
          collect_errors,
        );
        let $1 = $[1];
        if ($1 instanceof $Empty) {
          let $2 = $[0];
          if ($2 instanceof Some) {
            let vfs = $2[0];
            return new Ok([path, new Some(vfs)]);
          } else {
            if (ignore_initial_missing$1) {
              return new Ok([path, new None()]);
            } else {
              return new Err(toList([[path, new Enoent()]]));
            }
          }
        } else {
          let errors = $1;
          return new Err(errors);
        }
      },
    ),
    (roots) => {
      let state = [roots, initial];
      let stop$1 = repeatedly(
        interval$1,
        state,
        (state) => {
          let roots$1;
          let state$1;
          roots$1 = state[0];
          state$1 = state[1];
          return $list.fold(
            roots$1,
            [toList([]), state$1],
            (_use0, _use1) => {
              let roots$2;
              let state$2;
              roots$2 = _use0[0];
              state$2 = _use0[1];
              let path;
              let vfs;
              path = _use1[0];
              vfs = _use1[1];
              if (vfs instanceof Some) {
                let vfs$1 = vfs[0];
                let $ = diff(filter$1, max_depth$1, path, vfs$1, state$2, emit);
                let new_vfs;
                let state$3;
                new_vfs = $[0];
                state$3 = $[1];
                return [listPrepend([path, new_vfs], roots$2), state$3];
              } else {
                let $ = create(filter$1, max_depth$1, path, "", state$2, emit);
                let new_vfs;
                let state$3;
                new_vfs = $[0];
                state$3 = $[1];
                return [listPrepend([path, new_vfs], roots$2), state$3];
              }
            },
          );
        },
      );
      return new Ok(new Watcher(stop$1));
    },
  );
}

/**
 * Tell Polly to start watching all the specified directories for changes.
 * 
 * The callback is called synchronously while collecting change events since
 * the last run. It is adviseable to move heavier cpu-bound tasks from this
 * callback into their own processes or threads.
 * 
 * When running on the Erlang target, this spawns a new process.
 */
export function watch(options, emit) {
  return watch_with(
    options,
    undefined,
    (_, event) => {
      emit(event);
      return undefined;
    },
  );
}
