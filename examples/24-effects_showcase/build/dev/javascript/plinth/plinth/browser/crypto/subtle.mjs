import * as $array from "../../../../gleam_javascript/gleam/javascript/array.mjs";
import * as $promise from "../../../../gleam_javascript/gleam/javascript/promise.mjs";
import * as $json from "../../../../gleam_json/gleam/json.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import { toList, CustomType as $CustomType } from "../../../gleam.mjs";
import {
  digest as do_digest,
  exportJwk as export_jwk,
  generateKey as do_generate_key,
  importKey as do_import_key,
  importJwk as do_import_jwk,
  sign as do_sign,
  verify as do_verify,
} from "../../../plinth_browser_crypto_subtle_ffi.mjs";

export { export_jwk };

export class SHA1 extends $CustomType {}

export class SHA256 extends $CustomType {}

export class SHA384 extends $CustomType {}

export class SHA512 extends $CustomType {}

export class RsaHashedKeyGenParams extends $CustomType {
  constructor(name, modulus_length, public_exponent, hash) {
    super();
    this.name = name;
    this.modulus_length = modulus_length;
    this.public_exponent = public_exponent;
    this.hash = hash;
  }
}

export class EcKeyGenParams extends $CustomType {
  constructor(name, named_curve) {
    super();
    this.name = name;
    this.named_curve = named_curve;
  }
}

export class Encrypt extends $CustomType {}

export class Decrypt extends $CustomType {}

export class Sign extends $CustomType {}

export class Verify extends $CustomType {}

export class DeriveKey extends $CustomType {}

export class DeriveBits extends $CustomType {}

export class WrapKey extends $CustomType {}

export class UnwrapKey extends $CustomType {}

export class RsaHashedImportParams extends $CustomType {
  constructor(name, hash) {
    super();
    this.name = name;
    this.hash = hash;
  }
}

export class EcKeyImportParams extends $CustomType {
  constructor(name, named_curve) {
    super();
    this.name = name;
    this.named_curve = named_curve;
  }
}

export class HmacImportParams extends $CustomType {
  constructor(hash) {
    super();
    this.hash = hash;
  }
}

export class OtherImportParams extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}

export class P256 extends $CustomType {}

export class P384 extends $CustomType {}

export class P521 extends $CustomType {}

export class RsaSsaPkcs1v15 extends $CustomType {}

export class RsaPssParams extends $CustomType {
  constructor(salt_length) {
    super();
    this.salt_length = salt_length;
  }
}

export class EcdsaParams extends $CustomType {
  constructor(hash) {
    super();
    this.hash = hash;
  }
}

export class Hmac extends $CustomType {}

export class Ed25519 extends $CustomType {}

export function digest_algorithm_to_string(algorithm) {
  if (algorithm instanceof SHA1) {
    return "SHA-1";
  } else if (algorithm instanceof SHA256) {
    return "SHA-256";
  } else if (algorithm instanceof SHA384) {
    return "SHA-384";
  } else {
    return "SHA-512";
  }
}

export function digest(algorithm, data) {
  return do_digest(digest_algorithm_to_string(algorithm), data);
}

function key_usage_to_string(key_usage) {
  if (key_usage instanceof Encrypt) {
    return "encrypt";
  } else if (key_usage instanceof Decrypt) {
    return "decrypt";
  } else if (key_usage instanceof Sign) {
    return "sign";
  } else if (key_usage instanceof Verify) {
    return "verify";
  } else if (key_usage instanceof DeriveKey) {
    return "deriveKey";
  } else if (key_usage instanceof DeriveBits) {
    return "deriveBits";
  } else if (key_usage instanceof WrapKey) {
    return "wrapKey";
  } else {
    return "unwrapKey";
  }
}

export function generate_key(algorithm, extractable, key_usages) {
  let _block;
  let _pipe = key_usages;
  let _pipe$1 = $list.map(_pipe, key_usage_to_string);
  _block = $array.from_list(_pipe$1);
  let key_usages$1 = _block;
  return do_generate_key(algorithm, extractable, key_usages$1);
}

function named_curve_to_string(named_curve) {
  if (named_curve instanceof P256) {
    return "P-256";
  } else if (named_curve instanceof P384) {
    return "P-384";
  } else {
    return "P-521";
  }
}

function import_algorithm_to_json(algorithm_parameters) {
  if (algorithm_parameters instanceof RsaHashedImportParams) {
    let name = algorithm_parameters.name;
    let hash = algorithm_parameters.hash;
    return $json.object(
      toList([
        ["name", $json.string(name)],
        ["hash", $json.string(digest_algorithm_to_string(hash))],
      ]),
    );
  } else if (algorithm_parameters instanceof EcKeyImportParams) {
    let name = algorithm_parameters.name;
    let named_curve = algorithm_parameters.named_curve;
    return $json.object(
      toList([
        ["name", $json.string(name)],
        ["namedCurve", $json.string(named_curve_to_string(named_curve))],
      ]),
    );
  } else if (algorithm_parameters instanceof HmacImportParams) {
    let hash = algorithm_parameters.hash;
    return $json.object(
      toList([
        ["name", $json.string("HMAC")],
        ["hash", $json.string(digest_algorithm_to_string(hash))],
      ]),
    );
  } else {
    let name = algorithm_parameters.name;
    return $json.object(toList([["name", $json.string(name)]]));
  }
}

export function import_key(format, key_data, algorithm, extractable, key_usages) {
  let algorithm$1 = import_algorithm_to_json(algorithm);
  let _block;
  let _pipe = key_usages;
  let _pipe$1 = $list.map(_pipe, key_usage_to_string);
  _block = $array.from_list(_pipe$1);
  let key_usages$1 = _block;
  return do_import_key(format, key_data, algorithm$1, extractable, key_usages$1);
}

export function import_jwk(key_data, algorithm, extractable, key_usages) {
  let algorithm$1 = import_algorithm_to_json(algorithm);
  let _block;
  let _pipe = key_usages;
  let _pipe$1 = $list.map(_pipe, key_usage_to_string);
  _block = $array.from_list(_pipe$1);
  let key_usages$1 = _block;
  return do_import_jwk(key_data, algorithm$1, extractable, key_usages$1);
}

function sign_algorithm_to_json(key_algorithm) {
  if (key_algorithm instanceof RsaSsaPkcs1v15) {
    return $json.object(toList([["name", $json.string("RSASSA-PKCS1-v1_5")]]));
  } else if (key_algorithm instanceof RsaPssParams) {
    let salt_length = key_algorithm.salt_length;
    return $json.object(
      toList([
        ["name", $json.string("RSA-PSS")],
        ["saltLength", $json.int(salt_length)],
      ]),
    );
  } else if (key_algorithm instanceof EcdsaParams) {
    let hash = key_algorithm.hash;
    return $json.object(
      toList([
        ["name", $json.string("ECDSA")],
        ["hash", $json.string(digest_algorithm_to_string(hash))],
      ]),
    );
  } else if (key_algorithm instanceof Hmac) {
    return $json.object(toList([["name", $json.string("HMAC")]]));
  } else {
    return $json.object(toList([["name", $json.string("Ed25519")]]));
  }
}

export function sign(algorithm, key, data) {
  let algorithm$1 = sign_algorithm_to_json(algorithm);
  return do_sign(algorithm$1, key, data);
}

export function verify(algorithm, key, signature, data) {
  let algorithm$1 = sign_algorithm_to_json(algorithm);
  return do_verify(algorithm$1, key, signature, data);
}
