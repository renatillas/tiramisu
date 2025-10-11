import { spawn, kill, stdin } from "../../child_process_ffi.mjs";
import * as $stream from "../../plinth/node/stream.mjs";
import { exec } from "child_process";

export { exec, kill, spawn, stdin };
