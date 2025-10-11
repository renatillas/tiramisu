# polly

[![Package Version](https://img.shields.io/hexpm/v/polly)](https://hex.pm/packages/polly)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/polly/)

Polly is a portable, polling file-system watcher, supporting all targets that [simplifile](https://hexdocs.pm/simplifile/simplifile.html) supports.

While she may not be the fastest, she will help you in all situations where the [fs package](https://hex.pm/packages/fs) might not be suitable or supported. When needing to watch files over the network, or in situations where NIFs pose a security risk, cannot be compiled, or external dependencies like `inotify-tools` cannot be relied upon, Polly is here to help.

```sh
gleam add polly@2
```

```gleam
import gleam/io
import polly

pub fn main() {
  polly.new()
  |> polly.add_dir("src")
  |> polly.interval(1000)
  |> polly.watch(fn(event) {
    case event {
      Changed(path) -> io.println("CHANGED " <> path)
      Created(path) -> io.println("CREATED " <> path)
      Deleted(path) -> io.println("DELETED " <> path)
      Error(_, _) -> Nil
    }
  })
}
```

### Performance

Compared to the [fs](https://hex.pm/packages/fs) package, Polly does not use specialised operating system APIs like [inotify](https://www.man7.org/linux/man-pages/man7/inotify.7.html) or [ReadDirectoryChangesW](https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw). Instead, she recursively walks the specified directories every time the configured interval elapses, and compares the timestamps of all files with their previous timestamps that she needs to remember. Instead of having to keep around a single integer per directory, Polly stores all the file and directory names and their last timestamps, basically keeping around a "virtual" copy of the state of the filesystem.

This approach is fine if you're planning to watch single files or configuration/source directories, but even just forgetting to filter out the `build` directory can significantly slow down Polly and increase her CPU and memory usage a lot. **Never** attempt to use Polly to watch the entire file system, or even just the home directory of the user.

Always make sure you can't use other approaches first, and that you watch the smallest possible set of directories while having good [filter](./polly.html#filter) and [max_depth](./polly.html#max_depth) settings.

