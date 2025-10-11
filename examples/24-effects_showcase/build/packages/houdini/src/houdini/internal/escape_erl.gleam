@target(erlang)
pub fn escape(text: String) -> String {
  // This version is highly optimised for the Erlang target, it treats Strings
  // as BitArrays and slices them to share as much as possible. You can find
  // more details in `do_escape`.
  let bits = <<text:utf8>>

  let result = do_escape(bits, 0, bits, <<>>)

  // we know that the BitArray we build is definitely a valid string, so we can
  // skip verifying that again as well as dealing with the `Result`.
  coerce(result)
}

@target(erlang)
@external(erlang, "houdini_ffi", "coerce")
fn coerce(bit_array: a) -> b

// A possible way to escape chars would be to split the string into graphemes,
// traverse those one by one and accumulate them back into a string escaping
// ">", "<", etc. as we see them.
// However, we can be a lot more performant by working directly on the
// `BitArray` used to represent a Gleam UTF-8 String: instead of popping a
// grapheme at a time, we can work directly on BitArray slices: this has the big
// advantage of making sure we share as much as possible with the original
// string without having to build a new one from scratch.
@target(erlang)
fn do_escape(
  bin: BitArray,
  skip: Int,
  original: BitArray,
  acc: BitArray,
) -> BitArray {
  case bin {
    // If we find a char to escape we just advance the `skip` counter so that
    // it will be ignored in the following slice, then we append the escaped
    // version to the accumulator.
    <<"<", rest:bits>> -> {
      let acc = <<acc:bits, "&lt;">>
      do_escape(rest, skip + 1, original, acc)
    }

    <<">", rest:bits>> -> {
      let acc = <<acc:bits, "&gt;">>
      do_escape(rest, skip + 1, original, acc)
    }

    <<"&", rest:bits>> -> {
      let acc = <<acc:bits, "&amp;">>
      do_escape(rest, skip + 1, original, acc)
    }

    <<"\"", rest:bits>> -> {
      let acc = <<acc:bits, "&quot;">>
      do_escape(rest, skip + 1, original, acc)
    }

    <<"'", rest:bits>> -> {
      let acc = <<acc:bits, "&#39;">>
      do_escape(rest, skip + 1, original, acc)
    }

    // For any other byte that doesn't need to be escaped we go into an inner
    // loop, consuming as much "non-escapable" chars as possible.
    <<_char, rest:bits>> -> do_escape_normal(rest, skip, original, acc, 1)

    <<>> -> acc

    _ ->
      panic as "do_escape: non byte aligned string, all strings should be byte aligned"
  }
}

@target(erlang)
fn do_escape_normal(
  bin: BitArray,
  skip: Int,
  original: BitArray,
  acc: BitArray,
  len: Int,
) -> BitArray {
  // Remember, if we're here it means we've found a char that doesn't need to be
  // escaped, so what we want to do is advance the `len` counter until we reach
  // a char that _does_ need to be escaped and take the slice going from
  // `skip`, with size `len`.
  //
  // Imagine we're escaping this string: "abc<def&ghi" and we've reached 'd':
  // ```
  //    abc<def&ghi
  //       ^ `skip` points here
  // ```
  // We're going to be increasing `len` until we reach the '&':
  // ```
  //    abc<def&ghi
  //        ^^^ len will be 3 when we reach the '&' that needs escaping
  // ```
  // So we take the slice corresponding to "def".
  //
  case bin {
    // If we reach a char that has to be escaped we append the slice starting
    // from `skip` with size `len` and the escaped char.
    // This is what allows us to share as much of the original string as
    // possible: we only allocate a new BitArray for the escaped chars,
    // everything else is just a slice of the original String.
    <<"<", rest:bits>> -> {
      let acc = <<acc:bits, slice(original, skip, len):bits, "&lt;">>
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<">", rest:bits>> -> {
      let acc = <<acc:bits, slice(original, skip, len):bits, "&gt;">>
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<"&", rest:bits>> -> {
      let acc = <<acc:bits, slice(original, skip, len):bits, "&amp;">>
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<"\"", rest:bits>> -> {
      let acc = <<acc:bits, slice(original, skip, len):bits, "&quot;">>
      do_escape(rest, skip + len + 1, original, acc)
    }

    <<"'", rest:bits>> -> {
      let acc = <<acc:bits, slice(original, skip, len):bits, "&#39;">>
      do_escape(rest, skip + len + 1, original, acc)
    }

    // Otherwise we know that the first byte doesn't need any escape. The easy
    // thing to do would be to just advance by that one byte and keep going over
    // over the string. As you might notice here we're doing something a bit
    // more involved: we look at the following 7 bytes, and if none of those
    // needs escaping, then we skip this whole chunk of bytes entirely.
    //
    // This doesn't change the behaviour of the program, but it can make it a
    // whole load faster to go over the entire string. Especially if there's
    // fewer characters that need escaping!
    //
    // This idea comes from the amazing talk "Engineering json - Achieving Top
    // Performance on the BEAM" by Michał Muskala at Code BEAM Europe 2024:
    // https://www.youtube.com/watch?v=Z0swkSXAPBE
    <<_, b, c, d, e, f, g, h, rest:bits>>
      if b != 34
      && b != 38
      && b != 39
      && b != 60
      && b != 62
      && c != 34
      && c != 38
      && c != 39
      && c != 60
      && c != 62
      && d != 34
      && d != 38
      && d != 39
      && d != 60
      && d != 62
      && e != 34
      && e != 38
      && e != 39
      && e != 60
      && e != 62
      && f != 34
      && f != 38
      && f != 39
      && f != 60
      && f != 62
      && g != 34
      && g != 38
      && g != 39
      && g != 60
      && g != 62
      && h != 34
      && h != 38
      && h != 39
      && h != 60
      && h != 62
    -> do_escape_normal(rest, skip, original, acc, len + 8)

    // However, if any of the following bytes needs escaping, we skip over just
    // the first byte!
    <<_, rest:bits>> -> do_escape_normal(rest, skip, original, acc, len + 1)

    <<>> ->
      // We start from the start of the bit array and have consumed everything
      // without finding a char that is not valid. This means that the entire
      // string doesn't need any escaping, we can just return it as is!
      case skip {
        0 -> original
        _ -> <<acc:bits, slice(original, skip, len):bits>>
      }

    _ ->
      panic as "do_escape_normal: non byte aligned string, all strings should be byte aligned"
  }
}

@target(erlang)
@external(erlang, "binary", "part")
fn slice(bit_array: BitArray, from: Int, size: Int) -> BitArray
