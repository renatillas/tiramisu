import * as $filepath from "../filepath/filepath.mjs";
import * as $bit_array from "../gleam_stdlib/gleam/bit_array.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $set from "../gleam_stdlib/gleam/set.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  toBitArray,
} from "./gleam.mjs";
import {
  fileInfo as file_info,
  linkInfo as link_info,
  delete_ as delete$,
  readBits as read_bits,
  writeBits as write_bits,
  appendBits as append_bits,
  isDirectory as is_directory,
  createDirectory as create_directory,
  createSymlink as create_symlink,
  createLink as create_link,
  readDirectory as read_directory,
  isFile as is_file,
  isSymlink as is_symlink,
  createDirAll as do_create_dir_all,
  copyFile as do_copy_file,
  renameFile as rename_file,
  renameFile as rename,
  setPermissionsOctal as set_permissions_octal,
  currentDirectory as current_directory,
} from "./simplifile_js.mjs";

export {
  append_bits,
  create_directory,
  create_link,
  create_symlink,
  current_directory,
  delete$,
  file_info,
  is_directory,
  is_file,
  is_symlink,
  link_info,
  read_bits,
  read_directory,
  rename,
  rename_file,
  set_permissions_octal,
  write_bits,
};

const FILEPATH = "src/simplifile.gleam";

export class Eacces extends $CustomType {}

export class Eagain extends $CustomType {}

export class Ebadf extends $CustomType {}

export class Ebadmsg extends $CustomType {}

export class Ebusy extends $CustomType {}

export class Edeadlk extends $CustomType {}

export class Edeadlock extends $CustomType {}

export class Edquot extends $CustomType {}

export class Eexist extends $CustomType {}

export class Efault extends $CustomType {}

export class Efbig extends $CustomType {}

export class Eftype extends $CustomType {}

export class Eintr extends $CustomType {}

export class Einval extends $CustomType {}

export class Eio extends $CustomType {}

export class Eisdir extends $CustomType {}

export class Eloop extends $CustomType {}

export class Emfile extends $CustomType {}

export class Emlink extends $CustomType {}

export class Emultihop extends $CustomType {}

export class Enametoolong extends $CustomType {}

export class Enfile extends $CustomType {}

export class Enobufs extends $CustomType {}

export class Enodev extends $CustomType {}

export class Enolck extends $CustomType {}

export class Enolink extends $CustomType {}

export class Enoent extends $CustomType {}

export class Enomem extends $CustomType {}

export class Enospc extends $CustomType {}

export class Enosr extends $CustomType {}

export class Enostr extends $CustomType {}

export class Enosys extends $CustomType {}

export class Enotblk extends $CustomType {}

export class Enotdir extends $CustomType {}

export class Enotsup extends $CustomType {}

export class Enxio extends $CustomType {}

export class Eopnotsupp extends $CustomType {}

export class Eoverflow extends $CustomType {}

export class Eperm extends $CustomType {}

export class Epipe extends $CustomType {}

export class Erange extends $CustomType {}

export class Erofs extends $CustomType {}

export class Espipe extends $CustomType {}

export class Esrch extends $CustomType {}

export class Estale extends $CustomType {}

export class Etxtbsy extends $CustomType {}

export class Exdev extends $CustomType {}

export class NotUtf8 extends $CustomType {}

/**
 * Any error not accounted for by this type
 */
export class Unknown extends $CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
}

export class FileInfo extends $CustomType {
  constructor(size, mode, nlinks, inode, user_id, group_id, dev, atime_seconds, mtime_seconds, ctime_seconds) {
    super();
    this.size = size;
    this.mode = mode;
    this.nlinks = nlinks;
    this.inode = inode;
    this.user_id = user_id;
    this.group_id = group_id;
    this.dev = dev;
    this.atime_seconds = atime_seconds;
    this.mtime_seconds = mtime_seconds;
    this.ctime_seconds = ctime_seconds;
  }
}

export class File extends $CustomType {}

export class Directory extends $CustomType {}

export class Symlink extends $CustomType {}

export class Other extends $CustomType {}

export class Read extends $CustomType {}

export class Write extends $CustomType {}

export class Execute extends $CustomType {}

export class FilePermissions extends $CustomType {
  constructor(user, group, other) {
    super();
    this.user = user;
    this.group = group;
    this.other = other;
  }
}

/**
 * Convert an error into a human-readable description
 * ## Example
 * ```gleam
 * let assert "Input/output error" = describe_error(Eio)
 * ```
 */
export function describe_error(error) {
  if (error instanceof Eacces) {
    return "Permission denied";
  } else if (error instanceof Eagain) {
    return "Resource temporarily unavailable";
  } else if (error instanceof Ebadf) {
    return "Bad file descriptor";
  } else if (error instanceof Ebadmsg) {
    return "Bad message";
  } else if (error instanceof Ebusy) {
    return "Resource busy";
  } else if (error instanceof Edeadlk) {
    return "Resource deadlock avoided";
  } else if (error instanceof Edeadlock) {
    return "Resource deadlock avoided";
  } else if (error instanceof Edquot) {
    return "Disc quota exceeded";
  } else if (error instanceof Eexist) {
    return "File exists";
  } else if (error instanceof Efault) {
    return "Bad address";
  } else if (error instanceof Efbig) {
    return "File too large";
  } else if (error instanceof Eftype) {
    return "Inappropriate file type or format";
  } else if (error instanceof Eintr) {
    return "Interrupted system call";
  } else if (error instanceof Einval) {
    return "Invalid argument";
  } else if (error instanceof Eio) {
    return "Input/output error";
  } else if (error instanceof Eisdir) {
    return "Is a directory";
  } else if (error instanceof Eloop) {
    return "Too many levels of symbolic links";
  } else if (error instanceof Emfile) {
    return "Too many open files";
  } else if (error instanceof Emlink) {
    return "Too many links";
  } else if (error instanceof Emultihop) {
    return "Multihop attempted";
  } else if (error instanceof Enametoolong) {
    return "File name too long";
  } else if (error instanceof Enfile) {
    return "Too many open files in system";
  } else if (error instanceof Enobufs) {
    return "No buffer space available";
  } else if (error instanceof Enodev) {
    return "Operation not supported by device";
  } else if (error instanceof Enolck) {
    return "No locks available";
  } else if (error instanceof Enolink) {
    return "Link has been severed";
  } else if (error instanceof Enoent) {
    return "No such file or directory";
  } else if (error instanceof Enomem) {
    return "Cannot allocate memory";
  } else if (error instanceof Enospc) {
    return "No space left on device";
  } else if (error instanceof Enosr) {
    return "No STREAM resources";
  } else if (error instanceof Enostr) {
    return "Not a STREAM";
  } else if (error instanceof Enosys) {
    return "Function not implemented";
  } else if (error instanceof Enotblk) {
    return "Block device required";
  } else if (error instanceof Enotdir) {
    return "Not a directory";
  } else if (error instanceof Enotsup) {
    return "Operation not supported";
  } else if (error instanceof Enxio) {
    return "Device not configured";
  } else if (error instanceof Eopnotsupp) {
    return "Operation not supported on socket";
  } else if (error instanceof Eoverflow) {
    return "Value too large to be stored in data type";
  } else if (error instanceof Eperm) {
    return "Operation not permitted";
  } else if (error instanceof Epipe) {
    return "Broken pipe";
  } else if (error instanceof Erange) {
    return "Result too large";
  } else if (error instanceof Erofs) {
    return "Read-only file system";
  } else if (error instanceof Espipe) {
    return "Illegal seek";
  } else if (error instanceof Esrch) {
    return "No such process";
  } else if (error instanceof Estale) {
    return "Stale NFS file handle";
  } else if (error instanceof Etxtbsy) {
    return "Text file busy";
  } else if (error instanceof Exdev) {
    return "Cross-device link";
  } else if (error instanceof NotUtf8) {
    return "File not UTF-8 encoded";
  } else {
    let inner = error.inner;
    return "Unknown error: " + inner;
  }
}

/**
 * Extract the file permissions from a given FileInfo value in their octal representation.
 *
 * ## Example
 * ```gleam
 * use info <- result.try(simplifile.file_info("src/app.gleam"))
 * simplifile.file_info_permissions_octal(info)
 * // --> 0o644
 * ```
 */
export function file_info_permissions_octal(file_info) {
  return $int.bitwise_and(file_info.mode, 0o777);
}

/**
 * Extract the file type from a given FileInfo value.
 *
 * ## Example
 * ```gleam
 * use info <- result.try(simplifile.file_info("src/app.gleam"))
 * simplifile.file_info_type(info)
 * // --> simplifile.File
 * ```
 */
export function file_info_type(file_info) {
  let $ = $int.bitwise_and(file_info.mode, 0o170000);
  if ($ === 0o100000) {
    return new File();
  } else if ($ === 0o40000) {
    return new Directory();
  } else if ($ === 0o120000) {
    return new Symlink();
  } else {
    return new Other();
  }
}

/**
 * Delete all files/directories specified in a list of paths.
 * Recursively deletes provided directories.
 * Does not return an error if one or more of the provided paths
 * do not exist.
 */
export function delete_all(loop$paths) {
  while (true) {
    let paths = loop$paths;
    if (paths instanceof $Empty) {
      return new Ok(undefined);
    } else {
      let path = paths.head;
      let rest = paths.tail;
      let $ = delete$(path);
      if ($ instanceof Ok) {
        loop$paths = rest;
      } else {
        let $1 = $[0];
        if ($1 instanceof Enoent) {
          loop$paths = rest;
        } else {
          return $;
        }
      }
    }
  }
}

/**
 * Read a files contents as a string
 * ## Example
 * ```gleam
 * let assert Ok(records) = read(from: "./users.csv")
 * ```
 */
export function read(filepath) {
  let $ = read_bits(filepath);
  if ($ instanceof Ok) {
    let bits = $[0];
    let $1 = $bit_array.to_string(bits);
    if ($1 instanceof Ok) {
      return $1;
    } else {
      return new Error(new NotUtf8());
    }
  } else {
    return $;
  }
}

/**
 * Write a string to a file at the given path
 * ## Example
 * ```gleam
 * let assert Ok(Nil) = write(to: "./hello_world.txt", contents: "Hello, World!")
 * ```
 */
export function write(filepath, contents) {
  let _pipe = contents;
  let _pipe$1 = $bit_array.from_string(_pipe);
  return write_bits(filepath, _pipe$1);
}

/**
 * Append a string to the contents of a file at the given path
 * ## Example
 * ```gleam
 * let assert Ok(Nil) = append(to: "./needs_more_text.txt", contents: "more text")
 * ```
 */
export function append(filepath, contents) {
  let _pipe = contents;
  let _pipe$1 = $bit_array.from_string(_pipe);
  return append_bits(filepath, _pipe$1);
}

/**
 * Creates an empty file at the given filepath. Returns an `Error(Eexist)`
 * if the file already exists.
 */
export function create_file(filepath) {
  let $ = (() => {
    let _pipe = filepath;
    return is_file(_pipe);
  })();
  let $1 = (() => {
    let _pipe = filepath;
    return is_directory(_pipe);
  })();
  if ($ instanceof Ok) {
    let $2 = $[0];
    if ($2) {
      return new Error(new Eexist());
    } else if ($1 instanceof Ok) {
      let $3 = $1[0];
      if ($3) {
        return new Error(new Eexist());
      } else {
        return write_bits(filepath, toBitArray([]));
      }
    } else {
      return write_bits(filepath, toBitArray([]));
    }
  } else if ($1 instanceof Ok) {
    let $2 = $1[0];
    if ($2) {
      return new Error(new Eexist());
    } else {
      return write_bits(filepath, toBitArray([]));
    }
  } else {
    return write_bits(filepath, toBitArray([]));
  }
}

/**
 * Recursively creates necessary directories for a given directory
 * path. Note that if you pass a path that "looks like" a file, i.e.
 * `./a/b.txt`, a folder named `b.txt` will be created, so be sure
 * to pass only the path to the required directory.
 */
export function create_directory_all(dirpath) {
  let is_abs = $filepath.is_absolute(dirpath);
  let _block;
  let _pipe = dirpath;
  let _pipe$1 = $filepath.split(_pipe);
  _block = $list.fold(_pipe$1, "", $filepath.join);
  let path = _block;
  let _block$1;
  if (is_abs) {
    _block$1 = "/" + path;
  } else {
    _block$1 = path;
  }
  let path$1 = _block$1;
  return do_create_dir_all(path$1 + "/");
}

/**
 * Copy a file at a given path to another path.
 * Note: destination should include the filename, not just the directory
 */
export function copy_file(src, dest) {
  let _pipe = do_copy_file(src, dest);
  return $result.replace(_pipe, undefined);
}

function do_copy_directory(src, dest) {
  return $result.try$(
    read_directory(src),
    (segments) => {
      let _pipe = segments;
      $list.each(
        _pipe,
        (segment) => {
          let src_path = $filepath.join(src, segment);
          let dest_path = $filepath.join(dest, segment);
          return $result.try$(
            file_info(src_path),
            (src_info) => {
              let $ = file_info_type(src_info);
              if ($ instanceof File) {
                return $result.try$(
                  read_bits(src_path),
                  (content) => {
                    let _pipe$1 = content;
                    return write_bits(dest_path, _pipe$1);
                  },
                );
              } else if ($ instanceof Directory) {
                return $result.try$(
                  create_directory(dest_path),
                  (_) => { return do_copy_directory(src_path, dest_path); },
                );
              } else if ($ instanceof Symlink) {
                return new Error(
                  new Unknown(
                    "This is an internal bug where the `file_info` is somehow returning info about a simlink. Please file an issue on the simplifile repo.",
                  ),
                );
              } else {
                return new Error(
                  new Unknown(
                    "Unknown file type (not file, directory, or simlink)",
                  ),
                );
              }
            },
          );
        },
      )
      return new Ok(undefined);
    },
  );
}

/**
 * Copy a directory recursively
 */
export function copy_directory(src, dest) {
  return $result.try$(
    create_directory_all(dest),
    (_) => { return do_copy_directory(src, dest); },
  );
}

/**
 * Copy a file or a directory to a new path. Copies directories recursively.
 * 
 * ### Performance Note 
 * This function does work to determine if the src path
 * points to a file or a directory. Consider using one of the the dedicated 
 * functions `copy_file` or `copy_directory` if you already know which one you need.
 */
export function copy(src, dest) {
  return $result.try$(
    file_info(src),
    (src_info) => {
      let $ = file_info_type(src_info);
      if ($ instanceof File) {
        return copy_file(src, dest);
      } else if ($ instanceof Directory) {
        return copy_directory(src, dest);
      } else if ($ instanceof Symlink) {
        return new Error(
          new Unknown(
            "This is an internal bug where the `file_info` is somehow returning info about a simlink. Please file an issue on the simplifile repo.",
          ),
        );
      } else {
        return new Error(
          new Unknown("Unknown file type (not file, directory, or simlink)"),
        );
      }
    },
  );
}

/**
 * Copy a directory recursively and then delete the old one.
 */
export function rename_directory(src, dest) {
  return $result.try$(
    copy_directory(src, dest),
    (_) => { return delete$(src); },
  );
}

/**
 * Clear the contents of a directory, deleting all files and directories within
 * but leaving the top level directory in place.
 */
export function clear_directory(path) {
  return $result.try$(
    read_directory(path),
    (paths) => {
      let _pipe = paths;
      let _pipe$1 = $list.map(
        _pipe,
        (_capture) => { return $filepath.join(path, _capture); },
      );
      return delete_all(_pipe$1);
    },
  );
}

/**
 * Returns a list of filepaths for every file in the directory, including nested
 * files.
 */
export function get_files(directory) {
  return $result.try$(
    read_directory(directory),
    (contents) => {
      return $list.try_fold(
        contents,
        toList([]),
        (acc, content) => {
          let path = $filepath.join(directory, content);
          return $result.try$(
            file_info(path),
            (info) => {
              let $ = file_info_type(info);
              if ($ instanceof File) {
                return new Ok(listPrepend(path, acc));
              } else if ($ instanceof Directory) {
                return $result.try$(
                  get_files(path),
                  (nested_files) => {
                    return new Ok($list.append(acc, nested_files));
                  },
                );
              } else {
                return new Ok(acc);
              }
            },
          );
        },
      );
    },
  );
}

function permission_to_integer(permission) {
  if (permission instanceof Read) {
    return 0o4;
  } else if (permission instanceof Write) {
    return 0o2;
  } else {
    return 0o1;
  }
}

function integer_to_permissions(integer) {
  let $ = $int.bitwise_and(integer, 7);
  if ($ === 7) {
    return $set.from_list(toList([new Read(), new Write(), new Execute()]));
  } else if ($ === 6) {
    return $set.from_list(toList([new Read(), new Write()]));
  } else if ($ === 5) {
    return $set.from_list(toList([new Read(), new Execute()]));
  } else if ($ === 3) {
    return $set.from_list(toList([new Write(), new Execute()]));
  } else if ($ === 4) {
    return $set.from_list(toList([new Read()]));
  } else if ($ === 2) {
    return $set.from_list(toList([new Write()]));
  } else if ($ === 1) {
    return $set.from_list(toList([new Execute()]));
  } else if ($ === 0) {
    return $set.new$();
  } else {
    throw makeError(
      "panic",
      FILEPATH,
      "simplifile",
      667,
      "integer_to_permissions",
      "`panic` expression evaluated.",
      {}
    )
  }
}

export function file_permissions_to_octal(permissions) {
  let make_permission_digit = (permissions) => {
    let _pipe = permissions;
    let _pipe$1 = $set.to_list(_pipe);
    let _pipe$2 = $list.map(_pipe$1, permission_to_integer);
    return $int.sum(_pipe$2);
  };
  return (make_permission_digit(permissions.user) * 64 + make_permission_digit(
    permissions.group,
  ) * 8) + make_permission_digit(permissions.other);
}

function octal_to_file_permissions(octal) {
  return new FilePermissions(
    (() => {
      let _pipe = octal;
      let _pipe$1 = $int.bitwise_shift_right(_pipe, 6);
      return integer_to_permissions(_pipe$1);
    })(),
    (() => {
      let _pipe = octal;
      let _pipe$1 = $int.bitwise_shift_right(_pipe, 3);
      return integer_to_permissions(_pipe$1);
    })(),
    (() => {
      let _pipe = octal;
      return integer_to_permissions(_pipe);
    })(),
  );
}

/**
 * Extract the `FilePermissions` from a given FileInfo value.
 */
export function file_info_permissions(file_info) {
  return octal_to_file_permissions(file_info_permissions_octal(file_info));
}

/**
 * Sets the permissions for a given file
 *
 * # Example
 * ```gleam
 * let all = set.from_list([Read, Write, Execute])
 * let all = FilePermissions(user: all, group: all, other: all)
 * let assert Ok(Nil) = set_permissions("./script.sh", all)
 * ```
 */
export function set_permissions(filepath, permissions) {
  return set_permissions_octal(filepath, file_permissions_to_octal(permissions));
}
