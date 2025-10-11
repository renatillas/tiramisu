import filepath
import gleam.{Error as Err}
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string
import simplifile.{type FileError, type FileInfo, Eacces, Enoent}

// -- TYPES --------------------------------------------------------------------

/// A filesystem event.
/// 
/// Polly is careful when handing you events, and makes sure that you always get
/// them in the right order, so if a dictionary with some files was created,
/// you will get the dictionary event first, before any children.
pub type Event {
  /// A new file or dictionary was created!
  Created(path: String)
  /// A file got modified!
  Changed(path: String)
  /// A file or dictionary got deleted :(
  Deleted(path: String)
  /// An unexpected simplifile error happened :(
  /// 
  /// Polly treats `Enoent` and `Eacces` errors as if the file got deleted,
  /// and will therefore never pass those to you.
  Error(path: String, reason: FileError)
}

/// Polly uses the builder pattern to construct a watcher.
/// 
/// The `has_watched_dirs` phantom type variable helps Polly keep track of
/// wether or not you've called `add_dir` or `add_file` yet. That way, it will
/// be a type error if you try to watch nothing!
/// 
/// ```gleam
/// let options: Options = polly.new()
///     |> polly.add_dir("src")
///     |> polly.interval(3000)
///     |> polly.max_depth(10)
///     |> polly.filter(polly.default_filter)
/// ```
pub opaque type Options(has_watched_dirs) {
  Options(
    interval: Int,
    paths: List(String),
    max_depth: Int,
    filter: Filter,
    ignore_initial_missing: Bool,
  )
}

type Filter =
  fn(simplifile.FileType, String) -> Bool

/// A type that Polly uses to tag her options, indicating that you haven't
/// called `add_dir` or `add_file` yet.
pub type NoWatchedDirs

/// A type that Polly uses to tag her options, indicating that you have
/// called `add_dir` or `add_file`.
pub type HasWatchedDirs

/// A polling file system watcher, built from options.
/// 
/// Watchers are automatically started, and can by stopped by calling `stop`.
pub opaque type Watcher {
  Watcher(stop: fn() -> Nil)
}

// -- OPTIONS BUILDER ----------------------------------------------------------

/// Start creating a new configuration using the default options.
/// 
/// By default, an interval of 1 second is set, and the `default_filter` is used.
pub fn new() -> Options(NoWatchedDirs) {
  Options(
    interval: 1000,
    paths: [],
    max_depth: -1,
    filter: default_filter,
    ignore_initial_missing: False,
  )
}

/// Tell Polly which directory to watch. If it does not exist, `watch` will return an error.
/// 
/// If the directory goes away after watching has started, Polly will continue to
/// check on it to see if it came back.
/// 
/// Paths are not expanded by default, so the paths reported by events and passed
/// to the filter function will be prefixed with whatever you specified here.
pub fn add_dir(options: Options(a), path: String) -> Options(HasWatchedDirs) {
  let Options(interval:, paths:, max_depth:, filter:, ignore_initial_missing:) =
    options
  Options(
    paths: [path, ..paths],
    interval:,
    max_depth:,
    filter:,
    ignore_initial_missing:,
  )
}

/// Tell Polly to watch a single file.
/// 
/// Polly doesn't care if you tell her to watch a file or directory, but
/// using this function makes your intent clearer!
pub fn add_file(options: Options(a), path: String) -> Options(HasWatchedDirs) {
  add_dir(options, path)
}

/// Limit the maximum depth that Polly will walk each directory.
///
/// A limit of `0` would mean that Polly _only_ watches the specified list of
/// files or directories. A limit of `1` means that she will also look at the
/// files inside the given directories, but not at any nested directories.
/// 
/// There is no limit by default, but setting a limit might be good to
/// better control resource usage of the watcher.
/// 
/// Calling this function multiple times will cause polly to only remember the
/// lowest limit provided.
pub fn max_depth(
  options: Options(has_watched_dirs),
  max_depth: Int,
) -> Options(has_watched_dirs) {
  case options.max_depth < 0 || max_depth < options.max_depth {
    True -> Options(..options, max_depth: max_depth)
    False -> options
  }
}

/// Set the interval in-between file-system polls, in milliseconds.
/// 
/// This is the time that Polly rests between calls, so if scanning your directory
/// tree takes 100ms, and you configured 1000ms here, the total time between calls
/// will roughly be 1100ms.
/// 
/// Doing it this way makes sure that Polly doesn't stumble over herself.
pub fn interval(
  options: Options(has_watched_dirs),
  interval: Int,
) -> Options(has_watched_dirs) {
  case interval > 0 {
    True -> Options(..options, interval: interval)
    False -> options
  }
}

/// Filter files using the given predicate.
/// 
/// Polly will ignore files and directories for which the predicate returns `False`
/// completely, and any event happening for them or for a contained file of them
/// will not get reported.
/// 
/// Keep in mind that the filter is checked for every part of a path, not just
/// leaf nodes! So for example, if you have a path `./src/app.gleam`, your filter
/// function will first be called on `.`, then on `./src`, and then finally on
/// `./src/app.gleam`.
/// 
/// By default, all hidden files are ignored by using the `default_filter`.
pub fn filter(
  options: Options(has_watched_dirs),
  by filter: Filter,
) -> Options(has_watched_dirs) {
  Options(..options, filter: filter)
}

/// The default filter function, ignoring hidden files starting with a colon `"."`
pub fn default_filter(_type: simplifile.FileType, path: String) -> Bool {
  case filepath.base_name(path) {
    "." | ".." -> True
    basename -> !string.starts_with(basename, ".")
  }
}

/// Tell Polly that it is fine if a file or directory does not exist initially.
/// 
/// By default, if a file or directory cannot be found when calling `watch`,
/// Polly will immediately return to you with an `Enoent` error and refuse to run.
/// 
/// When this option is active, Polly will instead note the missing directory,
/// and continuously check if it appears, similarly to how she does after a
/// file or directory goes away after she has first seen it.
pub fn ignore_initial_missing(
  options: Options(has_watched_dirs),
) -> Options(has_watched_dirs) {
  Options(..options, ignore_initial_missing: True)
}

// -- WATCH API ----------------------------------------------------------------

type State(state) =
  #(List(#(String, Option(Vfs))), state)

/// Tell Polly to start watching all the specified directories for changes.
/// 
/// The callback is called synchronously while collecting change events since
/// the last run. It is adviseable to move heavier cpu-bound tasks from this
/// callback into their own processes or threads.
/// 
/// When running on the Erlang target, this spawns a new process.
pub fn watch(
  options: Options(HasWatchedDirs),
  with emit: fn(Event) -> ignored,
) -> Result(Watcher, List(#(String, FileError))) {
  watch_with(options, Nil, fn(_, event) {
    emit(event)
    Nil
  })
}

/// /// Like `watch`, but similar to `list.fold` Polly will also keep some state
/// around for you and pass it back on each invocation.
pub fn watch_with(
  options: Options(HasWatchedDirs),
  from initial: state,
  with emit: fn(state, Event) -> state,
) -> Result(Watcher, List(#(String, FileError))) {
  let Options(interval:, paths:, max_depth:, filter:, ignore_initial_missing:) =
    options

  use roots <- result.try({
    use path <- list.try_map(paths)
    case create(filter, max_depth, path, "", [], collect_errors) {
      #(Some(vfs), []) -> Ok(#(path, Some(vfs)))
      // if the directory is not there on hydrate, this is an error by default.
      #(None, []) ->
        case ignore_initial_missing {
          False -> Err([#(path, Enoent)])
          True -> Ok(#(path, None))
        }
      #(_, errors) -> Err(errors)
    }
  })

  let state: State(state) = #(roots, initial)

  let stop =
    repeatedly(interval, state, fn(state) {
      let #(roots, state) = state
      use #(roots, state), #(path, vfs) <- list.fold(roots, #([], state))

      case vfs {
        Some(vfs) -> {
          let #(new_vfs, state) =
            diff(filter, max_depth, path, vfs, state, emit)

          #([#(path, new_vfs), ..roots], state)
        }

        // check if the directory came back!
        option.None -> {
          let #(new_vfs, state) =
            create(filter, max_depth, path, "", state, emit)

          #([#(path, new_vfs), ..roots], state)
        }
      }
    })

  Ok(Watcher(stop))
}

fn collect_errors(
  errors: List(#(String, FileError)),
  event: Event,
) -> List(#(String, FileError)) {
  case event {
    Error(path, reason) -> [#(path, reason), ..errors]
    _ -> errors
  }
}

/// Stop this watcher.
/// 
/// If Polly currently scans your directories, she might not hear you right away
/// and may still report events for one run, after which she will stop.
pub fn stop(watcher: Watcher) -> Nil {
  watcher.stop()
}

// -- IMPLEMENTATION -----------------------------------------------------------

type Vfs {
  File(name: String, modkey: Int)
  Folder(name: String, modkey: Int, children: List(Vfs))
}

fn create(
  filter: Filter,
  depth: Int,
  path: String,
  name: String,
  state: state,
  emit: fn(state, Event) -> state,
) -> #(Option(Vfs), state) {
  let full_path = filepath.join(path, name)

  case simplifile.link_info(full_path) {
    Ok(stat) -> create_stat(filter, depth, name, full_path, stat, state, emit)
    Err(Enoent) | Err(Eacces) -> #(None, state)
    Err(reason) -> #(None, emit(state, Error(full_path, reason)))
  }
}

fn create_stat(
  filter: Filter,
  depth: Int,
  name: String,
  full_path: String,
  stat: FileInfo,
  state: state,
  emit: fn(state, Event) -> state,
) -> #(Option(Vfs), state) {
  let type_ = simplifile.file_info_type(stat)
  use <- bool.guard(when: !filter(type_, full_path), return: #(None, state))

  case type_ {
    simplifile.File -> #(
      Some(File(name, get_modkey(stat))),
      emit(state, Created(full_path)),
    )

    simplifile.Directory if depth == 0 -> #(
      Some(Folder(name, get_modkey(stat), [])),
      emit(state, Created(full_path)),
    )

    simplifile.Directory if depth != 0 -> {
      case readdir(full_path) {
        Ok(entries) -> {
          let depth = depth - 1
          let state = emit(state, Created(full_path))
          let #(children, state) =
            create_children(filter, depth, full_path, entries, [], state, emit)
          #(Some(Folder(name, get_modkey(stat), children)), state)
        }

        Err(Enoent) | Err(Eacces) -> #(None, state)
        Err(reason) -> #(None, emit(state, Error(full_path, reason)))
      }
    }

    _ -> #(None, state)
  }
}

fn create_children(
  filter: Filter,
  depth: Int,
  path: String,
  children: List(String),
  oks: List(Vfs),
  state: state,
  emit: fn(state, Event) -> state,
) -> #(List(Vfs), state) {
  case children {
    [] -> #(list.reverse(oks), state)
    [first, ..rest] ->
      case create(filter, depth, path, first, state, emit) {
        #(Some(vfs), state) ->
          create_children(filter, depth, path, rest, [vfs, ..oks], state, emit)

        #(None, state) ->
          create_children(filter, depth, path, rest, oks, state, emit)
      }
  }
}

fn diff(
  filter: Filter,
  depth: Int,
  path: String,
  vfs: Vfs,
  state: state,
  emit: fn(state, Event) -> state,
) -> #(Option(Vfs), state) {
  let full_path = filepath.join(path, vfs.name)
  case simplifile.link_info(full_path) {
    Ok(stat) -> {
      let type_ = simplifile.file_info_type(stat)
      case filter(type_, full_path) {
        True ->
          case type_, vfs {
            simplifile.File, File(name:, modkey: old_key) -> {
              let new_key = get_modkey(stat)
              case new_key == old_key {
                // hurray for reuse
                True -> #(Some(vfs), state)
                False -> {
                  #(Some(File(name, new_key)), emit(state, Changed(full_path)))
                }
              }
            }

            simplifile.Directory, Folder(..) if depth == 0 -> {
              // NOTE: we do not send change events for directories, so there is nothing to do.
              #(Some(vfs), state)
            }

            simplifile.Directory, Folder(name:, children: old_children, ..)
              if depth != 0
            ->
              case readdir(full_path) {
                Ok(new_entries) -> {
                  let #(children, state) =
                    diff_children(
                      filter,
                      depth - 1,
                      full_path,
                      old_children,
                      new_entries,
                      [],
                      state,
                      emit,
                    )

                  #(Some(Folder(name, get_modkey(stat), children)), state)
                }

                Err(Enoent) | Err(Eacces) -> {
                  #(None, delete(path, vfs, state, emit))
                }

                Err(reason) -> {
                  #(Some(vfs), emit(state, Error(full_path, reason)))
                }
              }

            _, _ ->
              create_stat(
                filter,
                depth,
                vfs.name,
                full_path,
                stat,
                delete(path, vfs, state, emit),
                emit,
              )
          }

        // we had a VFS, but now we should skip it -> emit deletes
        False -> #(None, delete(path, vfs, state, emit))
      }
    }

    // enoent might happen if the file is deleted while we are scanning
    Err(Eacces) | Err(Enoent) -> {
      #(None, delete(path, vfs, state, emit))
    }

    Err(reason) -> #(Some(vfs), emit(state, Error(path, reason)))
  }
}

fn diff_children(
  filter: Filter,
  depth: Int,
  path: String,
  old_children: List(Vfs),
  new_entries: List(String),
  new_children: List(Vfs),
  state: state,
  emit: fn(state, Event) -> state,
) -> #(List(Vfs), state) {
  case old_children, new_entries {
    [], [] -> #(list.reverse(new_children), state)
    [first_old, ..rest_old], [first_new, ..rest_new] ->
      case string.compare(first_old.name, first_new) {
        order.Eq -> {
          case diff(filter, depth, path, first_old, state, emit) {
            #(Some(new_vfs), state) ->
              diff_children(
                filter,
                depth,
                path,
                rest_old,
                rest_new,
                [new_vfs, ..new_children],
                state,
                emit,
              )

            #(None, state) ->
              diff_children(
                filter,
                depth,
                path,
                rest_old,
                rest_new,
                new_children,
                state,
                emit,
              )
          }
        }

        order.Gt -> {
          // created a file
          let #(new_vfs, state) =
            create(filter, depth, path, first_new, state, emit)

          let new_children = case new_vfs {
            Some(new_vfs) -> [new_vfs, ..new_children]
            None -> new_children
          }

          diff_children(
            filter,
            depth,
            path,
            old_children,
            rest_new,
            new_children,
            state,
            emit,
          )
        }

        order.Lt ->
          // deleted a file
          diff_children(
            filter,
            depth,
            path,
            rest_old,
            new_entries,
            new_children,
            delete(path, first_old, state, emit),
            emit,
          )
      }

    [], [first_new, ..rest_new] -> {
      let #(new_vfs, state) =
        create(filter, depth, path, first_new, state, emit)

      let new_children = case new_vfs {
        Some(new_vfs) -> [new_vfs, ..new_children]
        None -> new_children
      }

      diff_children(
        filter,
        depth,
        path,
        old_children,
        rest_new,
        new_children,
        state,
        emit,
      )
    }

    [first_old, ..rest_old], [] ->
      // deleted all the remaining old ones
      diff_children(
        filter,
        depth,
        path,
        rest_old,
        new_entries,
        new_children,
        delete(path, first_old, state, emit),
        emit,
      )
  }
}

fn delete(
  path: String,
  vfs: Vfs,
  state: state,
  emit: fn(state, Event) -> state,
) -> state {
  let full_path = filepath.join(path, vfs.name)
  case vfs {
    File(..) -> emit(state, Deleted(full_path))
    Folder(children:, ..) -> {
      let state =
        list.fold(children, state, fn(state, child) {
          delete(full_path, child, state, emit)
        })
      emit(state, Deleted(full_path))
    }
  }
}

fn get_modkey(stat: simplifile.FileInfo) -> Int {
  int.max(stat.mtime_seconds, stat.ctime_seconds)
}

fn readdir(path: String) -> Result(List(String), FileError) {
  simplifile.read_directory(path)
  |> result.map(list.sort(_, by: string.compare))
}

// -- EXTERNAL -----------------------------------------------------------------

@external(erlang, "polly_ffi", "repeatedly")
@external(javascript, "./polly_ffi.mjs", "repeatedly")
fn repeatedly(
  timeout: Int,
  initial state: state,
  with callback: fn(state) -> state,
) -> fn() -> Nil
