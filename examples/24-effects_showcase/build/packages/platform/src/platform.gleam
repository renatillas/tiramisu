pub type Runtime {
  Erlang
  Node
  Bun
  Deno
  Browser
  OtherRuntime(String)
}

pub type Os {
  Aix
  Darwin
  FreeBsd
  Linux
  OpenBsd
  SunOs
  Win32
  OtherOs(String)
}

pub type Arch {
  Arm
  Arm64
  X86
  X64
  Loong64
  Mips
  MipsLittleEndian
  PPC
  PPC64
  RiscV64
  S390
  S390X
  OtherArch(String)
}

@external(erlang, "platform_ffi", "runtime")
@external(javascript, "./platform_ffi.mjs", "runtime")
fn runtime_() -> String

@external(erlang, "platform_ffi", "os")
@external(javascript, "./platform_ffi.mjs", "os")
fn os_() -> String

@external(erlang, "platform_ffi", "arch")
@external(javascript, "./platform_ffi.mjs", "arch")
fn arch_() -> String

/// Returns the runtime this application is running on
/// 
/// On the erlang target, it'll always return `Erlang`
/// 
/// On the js target, it'll try to detect the js runtime
pub fn runtime() -> Runtime {
  case runtime_() {
    "erlang" -> Erlang
    "node" -> Node
    "bun" -> Bun
    "deno" -> Deno
    "browser" -> Browser
    runtime -> OtherRuntime(runtime)
  }
}

/// Returns the host operating system this appication is running on
/// 
/// In web browsers, this will always return OtherOs("unknown")
pub fn os() -> Os {
  case os_() {
    "aix" -> Aix
    "darwin" -> Darwin
    "freebsd" -> FreeBsd
    "linux" -> Linux
    "openbsd" -> OpenBsd
    "sunos" -> SunOs
    "win32" | "win" <> _ -> Win32
    os -> OtherOs(os)
  }
}

/// Returns the CPU architecture of the host system
/// 
/// In web browsers, this will always return OtherArch("unknown")
/// 
/// On erlang for windows, it'll always return either X86 or X64, even under a beam vm compiled for arm.
/// This is because there is no simple way to get the cpu archictecture on windows, and it currently uses 
/// the bitness of the cpu to guess it instead. 
/// 
/// As of 22nd August 2024, there are no prebuilt binaries for windows for arm, so this shouldn't matter
pub fn arch() -> Arch {
  case arch_() {
    "arm" -> Arm
    "arm64" | "aarch64" -> Arm64
    "x86" | "ia32" -> X86
    "x64" | "x86_64" | "amd64" -> X64
    "loong64" -> Loong64
    "mips" -> Mips
    "mipsel" -> MipsLittleEndian
    "ppc" -> PPC
    "ppc64" -> PPC64
    "riscv64" -> RiscV64
    "s390" -> S390
    "s390x" -> S390X
    arch -> OtherArch(arch)
  }
}
