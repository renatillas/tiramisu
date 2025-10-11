import * as $httpc from "../../gleam_httpc/gleam/httpc.mjs";
import * as $actor from "../../gleam_otp/gleam/otp/actor.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../../simplifile/simplifile.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $system from "../lustre_dev_tools/system.mjs";

export class CouldNotDownloadBunBinary extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CouldNotDownloadTailwindBinary extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CouldNotExtractBunArchive extends $CustomType {
  constructor(os, arch, version) {
    super();
    this.os = os;
    this.arch = arch;
    this.version = version;
  }
}

export class CouldNotInitialiseDevTools extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CouldNotLocateBunBinary extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}

export class CouldNotLocateTailwindBinary extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}

export class CouldNotReadFile extends $CustomType {
  constructor(path, reason) {
    super();
    this.path = path;
    this.reason = reason;
  }
}

export class CouldNotSetFilePermissions extends $CustomType {
  constructor(path, reason) {
    super();
    this.path = path;
    this.reason = reason;
  }
}

export class CouldNotStartDevServer extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class CouldNotStartFileWatcher extends $CustomType {
  constructor(watcher, os, arch) {
    super();
    this.watcher = watcher;
    this.os = os;
    this.arch = arch;
  }
}

export class CouldNotVerifyBunHash extends $CustomType {
  constructor(expected, actual) {
    super();
    this.expected = expected;
    this.actual = actual;
  }
}

export class CouldNotVerifyTailwindHash extends $CustomType {
  constructor(expected, actual) {
    super();
    this.expected = expected;
    this.actual = actual;
  }
}

export class CouldNotWriteFile extends $CustomType {
  constructor(path, reason) {
    super();
    this.path = path;
    this.reason = reason;
  }
}

export class ExternalCommandFailed extends $CustomType {
  constructor(command, reason) {
    super();
    this.command = command;
    this.reason = reason;
  }
}

export class FailedToBuildProject extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

export class MissingRequiredFlag extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class MustBeProjectRoot extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}

export class ProxyInvalidTo extends $CustomType {}

export class ProxyMissingFrom extends $CustomType {}

export class ProxyMissingTo extends $CustomType {}

export class UnknownBuildTool extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class UnknownGleamModule extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class UnknownIntegration extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class UnsupportedBunVersion extends $CustomType {
  constructor(path, expected, actual) {
    super();
    this.path = path;
    this.expected = expected;
    this.actual = actual;
  }
}

export class UnsupportedPlatform extends $CustomType {
  constructor(os, arch) {
    super();
    this.os = os;
    this.arch = arch;
  }
}

export class UnsupportedTailwindVersion extends $CustomType {
  constructor(path, expected, actual) {
    super();
    this.path = path;
    this.expected = expected;
    this.actual = actual;
  }
}
