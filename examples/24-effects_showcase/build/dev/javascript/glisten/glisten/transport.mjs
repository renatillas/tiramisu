import * as $atom from "../../gleam_erlang/gleam/erlang/atom.mjs";
import * as $process from "../../gleam_erlang/gleam/erlang/process.mjs";
import * as $bytes_tree from "../../gleam_stdlib/gleam/bytes_tree.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $socket from "../glisten/socket.mjs";
import * as $options from "../glisten/socket/options.mjs";
import * as $ssl from "../glisten/ssl.mjs";
import * as $tcp from "../glisten/tcp.mjs";

export class Tcp extends $CustomType {}

export class Ssl extends $CustomType {}

function decode_ipv4() {
  return $decode.field(
    0,
    $decode.int,
    (a) => {
      return $decode.field(
        1,
        $decode.int,
        (b) => {
          return $decode.field(
            2,
            $decode.int,
            (c) => {
              return $decode.field(
                3,
                $decode.int,
                (d) => { return $decode.success(new $options.IpV4(a, b, c, d)); },
              );
            },
          );
        },
      );
    },
  );
}
