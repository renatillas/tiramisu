# directories

A lightweight gleam package to get standard locations of directories for temporary files, config, cache, etc.


[![Package Version](https://img.shields.io/hexpm/v/directories)](https://hex.pm/packages/directories)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/directories/)

```sh
gleam add directories@1
```
```gleam
import directories

pub fn main() {
  // All functions return an Result(String, Nil)
  // Nil will be returned whenever it couldn't find a valid directory
  let tmp_dir = io.debug(directories.tmp_dir())
  let home_dir = io.debug(directories.home_dir())
  let cache_dir = io.debug(directories.cache_dir())
  let config_dir = io.debug(directories.config_dir())
  let config_local_dir = io.debug(directories.config_local_dir())
  let data_dir = io.debug(directories.data_dir())
  let data_local_dir = io.debug(directories.data_local_dir())
  let executable_dir = io.debug(directories.executable_dir())
  let preferences_dir = io.debug(directories.preferences_dir())
  let runtime_dir = io.debug(directories.runtime_dir())
  let state_dir = io.debug(directories.state_dir())
}
```

Further documentation can be found at <https://hexdocs.pm/directories>.

## Dependencies
- It depends on `gleam_stdlib`
- It depends on `platform` to find out what is the host operating system.
- It depends on `simplifile` to check if a folder exists and is a valid file
- It depends on `envoy` to get environment variables

## TODO
- [x] Publish to hexpm
- [x] Remove dependency on `gleam_erlang` to make it work in the js runtime

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
