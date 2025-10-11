import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  CustomType as $CustomType,
  isEqual,
  toBitArray,
  bitArraySlice,
  stringBits,
} from "../gleam.mjs";
import {
  strongRandomBytes as strong_random_bytes,
  hashInit as new_hasher,
  hashUpdate as hash_chunk,
  digest,
  hmac,
} from "../gleam_crypto_ffi.mjs";

export { digest, hash_chunk, hmac, new_hasher, strong_random_bytes };

export class Sha224 extends $CustomType {}

export class Sha256 extends $CustomType {}

export class Sha384 extends $CustomType {}

export class Sha512 extends $CustomType {}

export class Md5 extends $CustomType {}

export class Sha1 extends $CustomType {}

/**
 * Computes a digest of the input bit string.
 *
 * ## Examples
 *
 * ```gleam
 * let digest = hash(Sha256, <<"a":utf8>>)
 * ```
 * If you wish to to hash content in multiple chunks rather than all at once
 * see the `new_hasher` function.
 */
export function hash(algorithm, data) {
  let _pipe = new_hasher(algorithm);
  let _pipe$1 = hash_chunk(_pipe, data);
  return digest(_pipe$1);
}

function do_secure_compare(loop$left, loop$right, loop$accumulator) {
  while (true) {
    let left = loop$left;
    let right = loop$right;
    let accumulator = loop$accumulator;
    if (
      right.bitSize >= 8 &&
      (right.bitSize - 8) % 8 === 0 &&
      left.bitSize >= 8 &&
      (left.bitSize - 8) % 8 === 0
    ) {
      let y = right.byteAt(0);
      let right$1 = bitArraySlice(right, 8);
      let x = left.byteAt(0);
      let left$1 = bitArraySlice(left, 8);
      let accumulator$1 = $int.bitwise_or(
        accumulator,
        $int.bitwise_exclusive_or(x, y),
      );
      loop$left = left$1;
      loop$right = right$1;
      loop$accumulator = accumulator$1;
    } else {
      return (isEqual(left, right)) && (accumulator === 0);
    }
  }
}

/**
 * Compares the two binaries in constant-time to avoid timing attacks.
 *
 * For more details see: http://codahale.com/a-lesson-in-timing-attacks/
 */
export function secure_compare(left, right) {
  let $ = $bit_array.byte_size(left) === $bit_array.byte_size(right);
  if ($) {
    return do_secure_compare(left, right, 0);
  } else {
    return $;
  }
}

function signing_input(digest_type, message) {
  let _block;
  if (digest_type instanceof Sha224) {
    _block = "HS224";
  } else if (digest_type instanceof Sha256) {
    _block = "HS256";
  } else if (digest_type instanceof Sha384) {
    _block = "HS384";
  } else if (digest_type instanceof Sha512) {
    _block = "HS512";
  } else if (digest_type instanceof Md5) {
    _block = "HMD5";
  } else {
    _block = "HS1";
  }
  let protected$ = _block;
  return $string.concat(
    toList([
      $bit_array.base64_url_encode(toBitArray([stringBits(protected$)]), false),
      ".",
      $bit_array.base64_url_encode(message, false),
    ]),
  );
}

/**
 * Sign a message which can later be verified using the `verify_signed_message`
 * function to detect if the message has been tampered with.
 *
 * A web application could use this verifier to sign HTTP cookies. The data can
 * be read by the user, but cannot be tampered with.
 */
export function sign_message(message, secret, digest_type) {
  let input = signing_input(digest_type, message);
  let signature = hmac(toBitArray([stringBits(input)]), digest_type, secret);
  return $string.concat(
    toList([input, ".", $bit_array.base64_url_encode(signature, false)]),
  );
}

/**
 * Verify a message created by the `sign_message` function.
 */
export function verify_signed_message(message, secret) {
  return $result.try$(
    (() => {
      let $ = $string.split(message, ".");
      if ($ instanceof $Empty) {
        return new Error(undefined);
      } else {
        let $1 = $.tail;
        if ($1 instanceof $Empty) {
          return new Error(undefined);
        } else {
          let $2 = $1.tail;
          if ($2 instanceof $Empty) {
            return new Error(undefined);
          } else {
            let $3 = $2.tail;
            if ($3 instanceof $Empty) {
              let a = $.head;
              let b = $1.head;
              let c = $2.head;
              return new Ok([a, b, c]);
            } else {
              return new Error(undefined);
            }
          }
        }
      }
    })(),
    (_use0) => {
      let protected$;
      let payload;
      let signature;
      protected$ = _use0[0];
      payload = _use0[1];
      signature = _use0[2];
      let text = $string.concat(toList([protected$, ".", payload]));
      return $result.try$(
        $bit_array.base64_url_decode(payload),
        (payload) => {
          return $result.try$(
            $bit_array.base64_url_decode(signature),
            (signature) => {
              return $result.try$(
                $bit_array.base64_url_decode(protected$),
                (protected$) => {
                  return $result.try$(
                    (() => {
                      if (
                        protected$.bitSize >= 8 &&
                        protected$.byteAt(0) === 72 &&
                        protected$.bitSize >= 16
                      ) {
                        if (protected$.byteAt(1) === 83) {
                          if (protected$.bitSize >= 24) {
                            if (protected$.byteAt(2) === 50) {
                              if (protected$.bitSize >= 32) {
                                if (protected$.byteAt(3) === 50) {
                                  if (protected$.bitSize === 40) {
                                    if (protected$.byteAt(4) === 52) {
                                      return new Ok(new Sha224());
                                    } else if (
                                      protected$.byteAt(3) === 53 &&
                                      protected$.byteAt(4) === 54
                                    ) {
                                      return new Ok(new Sha256());
                                    } else if (
                                      protected$.byteAt(2) === 53 &&
                                      protected$.byteAt(3) === 49 &&
                                      protected$.byteAt(4) === 50
                                    ) {
                                      return new Ok(new Sha512());
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else if (
                                    protected$.byteAt(1) === 77 &&
                                    protected$.byteAt(2) === 68 &&
                                    protected$.bitSize === 32 &&
                                    protected$.byteAt(3) === 53
                                  ) {
                                    return new Ok(new Md5());
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else if (protected$.byteAt(3) === 53) {
                                  if (protected$.bitSize === 40) {
                                    if (protected$.byteAt(4) === 54) {
                                      return new Ok(new Sha256());
                                    } else if (
                                      protected$.byteAt(2) === 51 &&
                                      protected$.byteAt(3) === 56 &&
                                      protected$.byteAt(4) === 52
                                    ) {
                                      return new Ok(new Sha384());
                                    } else if (
                                      protected$.byteAt(2) === 53 &&
                                      protected$.byteAt(3) === 49 &&
                                      protected$.byteAt(4) === 50
                                    ) {
                                      return new Ok(new Sha512());
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else if (
                                    protected$.byteAt(1) === 77 &&
                                    protected$.byteAt(2) === 68 &&
                                    protected$.bitSize === 32
                                  ) {
                                    return new Ok(new Md5());
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else if (
                                  protected$.byteAt(2) === 51 &&
                                  protected$.byteAt(3) === 56
                                ) {
                                  if (protected$.bitSize === 40) {
                                    if (protected$.byteAt(4) === 52) {
                                      return new Ok(new Sha384());
                                    } else if (
                                      protected$.byteAt(2) === 53 &&
                                      protected$.byteAt(3) === 49 &&
                                      protected$.byteAt(4) === 50
                                    ) {
                                      return new Ok(new Sha512());
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else if (
                                  protected$.byteAt(2) === 53 &&
                                  protected$.byteAt(3) === 49 &&
                                  protected$.bitSize === 40 &&
                                  protected$.byteAt(4) === 50
                                ) {
                                  return new Ok(new Sha512());
                                } else {
                                  return new Error(undefined);
                                }
                              } else if (
                                protected$.bitSize === 24 &&
                                protected$.byteAt(2) === 49
                              ) {
                                return new Ok(new Sha1());
                              } else {
                                return new Error(undefined);
                              }
                            } else if (protected$.byteAt(2) === 51) {
                              if (protected$.bitSize >= 32) {
                                if (protected$.byteAt(3) === 56) {
                                  if (protected$.bitSize === 40) {
                                    if (protected$.byteAt(4) === 52) {
                                      return new Ok(new Sha384());
                                    } else if (
                                      protected$.byteAt(2) === 53 &&
                                      protected$.byteAt(3) === 49 &&
                                      protected$.byteAt(4) === 50
                                    ) {
                                      return new Ok(new Sha512());
                                    } else {
                                      return new Error(undefined);
                                    }
                                  } else if (
                                    protected$.byteAt(1) === 77 &&
                                    protected$.byteAt(2) === 68 &&
                                    protected$.bitSize === 32 &&
                                    protected$.byteAt(3) === 53
                                  ) {
                                    return new Ok(new Md5());
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else if (
                                  protected$.byteAt(2) === 53 &&
                                  protected$.byteAt(3) === 49 &&
                                  protected$.bitSize === 40
                                ) {
                                  if (protected$.byteAt(4) === 50) {
                                    return new Ok(new Sha512());
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else if (
                                  protected$.byteAt(1) === 77 &&
                                  protected$.byteAt(2) === 68 &&
                                  protected$.bitSize === 32 &&
                                  protected$.byteAt(3) === 53
                                ) {
                                  return new Ok(new Md5());
                                } else {
                                  return new Error(undefined);
                                }
                              } else if (
                                protected$.bitSize === 24 &&
                                protected$.byteAt(2) === 49
                              ) {
                                return new Ok(new Sha1());
                              } else {
                                return new Error(undefined);
                              }
                            } else if (protected$.byteAt(2) === 53) {
                              if (protected$.bitSize >= 32) {
                                if (
                                  protected$.byteAt(3) === 49 &&
                                  protected$.bitSize === 40
                                ) {
                                  if (protected$.byteAt(4) === 50) {
                                    return new Ok(new Sha512());
                                  } else {
                                    return new Error(undefined);
                                  }
                                } else if (
                                  protected$.byteAt(1) === 77 &&
                                  protected$.byteAt(2) === 68 &&
                                  protected$.bitSize === 32 &&
                                  protected$.byteAt(3) === 53
                                ) {
                                  return new Ok(new Md5());
                                } else {
                                  return new Error(undefined);
                                }
                              } else if (
                                protected$.bitSize === 24 &&
                                protected$.byteAt(2) === 49
                              ) {
                                return new Ok(new Sha1());
                              } else {
                                return new Error(undefined);
                              }
                            } else if (protected$.bitSize === 24) {
                              if (protected$.byteAt(2) === 49) {
                                return new Ok(new Sha1());
                              } else {
                                return new Error(undefined);
                              }
                            } else if (
                              protected$.byteAt(1) === 77 &&
                              protected$.byteAt(2) === 68 &&
                              protected$.bitSize === 32 &&
                              protected$.byteAt(3) === 53
                            ) {
                              return new Ok(new Md5());
                            } else {
                              return new Error(undefined);
                            }
                          } else {
                            return new Error(undefined);
                          }
                        } else if (
                          protected$.byteAt(1) === 77 &&
                          protected$.bitSize >= 24 &&
                          protected$.byteAt(2) === 68 &&
                          protected$.bitSize === 32 &&
                          protected$.byteAt(3) === 53
                        ) {
                          return new Ok(new Md5());
                        } else {
                          return new Error(undefined);
                        }
                      } else {
                        return new Error(undefined);
                      }
                    })(),
                    (digest_type) => {
                      let challenge = hmac(
                        toBitArray([stringBits(text)]),
                        digest_type,
                        secret,
                      );
                      let $ = secure_compare(challenge, signature);
                      if ($) {
                        return new Ok(payload);
                      } else {
                        return new Error(undefined);
                      }
                    },
                  );
                },
              );
            },
          );
        },
      );
    },
  );
}
