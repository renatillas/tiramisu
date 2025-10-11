import * as $bit_array from "../../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $list from "../../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../../gleam_stdlib/gleam/result.mjs";
import * as $logging from "../../../../logging/logging.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  toBitArray,
  bitArraySlice,
  bitArraySliceToInt,
  sizedInt,
} from "../../../gleam.mjs";

const FILEPATH = "src/mist/internal/http2/frame.gleam";

class StreamIdentifier extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class HeaderPriority extends $CustomType {
  constructor(exclusive, stream_dependency, weight) {
    super();
    this.exclusive = exclusive;
    this.stream_dependency = stream_dependency;
    this.weight = weight;
  }
}

export class Complete extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Continued extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Enabled extends $CustomType {}

export class Disabled extends $CustomType {}

export class HeaderTableSize extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class ServerPush extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class MaxConcurrentStreams extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class InitialWindowSize extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class MaxFrameSize extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class MaxHeaderListSize extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export class Data extends $CustomType {
  constructor(data, end_stream, identifier) {
    super();
    this.data = data;
    this.end_stream = end_stream;
    this.identifier = identifier;
  }
}

export class Header extends $CustomType {
  constructor(data, end_stream, identifier, priority) {
    super();
    this.data = data;
    this.end_stream = end_stream;
    this.identifier = identifier;
    this.priority = priority;
  }
}

export class Priority extends $CustomType {
  constructor(exclusive, identifier, stream_dependency, weight) {
    super();
    this.exclusive = exclusive;
    this.identifier = identifier;
    this.stream_dependency = stream_dependency;
    this.weight = weight;
  }
}

export class Termination extends $CustomType {
  constructor(error, identifier) {
    super();
    this.error = error;
    this.identifier = identifier;
  }
}

export class Settings extends $CustomType {
  constructor(ack, settings) {
    super();
    this.ack = ack;
    this.settings = settings;
  }
}

export class PushPromise extends $CustomType {
  constructor(data, identifier, promised_stream_id) {
    super();
    this.data = data;
    this.identifier = identifier;
    this.promised_stream_id = promised_stream_id;
  }
}

export class Ping extends $CustomType {
  constructor(ack, data) {
    super();
    this.ack = ack;
    this.data = data;
  }
}

export class GoAway extends $CustomType {
  constructor(data, error, last_stream_id) {
    super();
    this.data = data;
    this.error = error;
    this.last_stream_id = last_stream_id;
  }
}

export class WindowUpdate extends $CustomType {
  constructor(amount, identifier) {
    super();
    this.amount = amount;
    this.identifier = identifier;
  }
}

export class Continuation extends $CustomType {
  constructor(data, identifier) {
    super();
    this.data = data;
    this.identifier = identifier;
  }
}

export class NoError extends $CustomType {}

export class ProtocolError extends $CustomType {}

export class InternalError extends $CustomType {}

export class FlowControlError extends $CustomType {}

export class SettingsTimeout extends $CustomType {}

export class StreamClosed extends $CustomType {}

export class FrameSizeError extends $CustomType {}

export class RefusedStream extends $CustomType {}

export class Cancel extends $CustomType {}

export class CompressionError extends $CustomType {}

export class ConnectError extends $CustomType {}

export class EnhanceYourCalm extends $CustomType {}

export class InadequateSecurity extends $CustomType {}

export class Http11Required extends $CustomType {}

export class Unsupported extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}

export function stream_identifier(value) {
  return new StreamIdentifier(value);
}

export function get_stream_identifier(identifier) {
  let value;
  value = identifier[0];
  return value;
}

function parse_data(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if ($.bitSize >= 4 && $.bitSize >= 5 && $.bitSize >= 7 && $.bitSize >= 8) {
    let padding = bitArraySliceToInt($, 4, 5, true, false);
    if ($.bitSize >= 8 + padding * 8 && identifier !== 0) {
      let padding$1 = padding;
      let end_stream = bitArraySliceToInt($, 7, 8, true, false);
      let pad_length = bitArraySliceToInt($, 8, 8 + padding$1 * 8, true, false);
      let data_and_padding = bitArraySlice($, 8 + padding * 8);
      let _block;
      if (padding$1 === 1) {
        _block = length - pad_length;
      } else if (padding$1 === 0) {
        _block = length;
      } else {
        throw makeError(
          "panic",
          FILEPATH,
          "mist/internal/http2/frame",
          168,
          "parse_data",
          "Somehow a bit was neither 0 nor 1",
          {}
        )
      }
      let data_length = _block;
      if (data_length * 8 >= 0 && data_and_padding.bitSize >= data_length * 8) {
        let data = bitArraySlice(data_and_padding, 0, data_length * 8);
        return new Ok(
          new Data(data, end_stream === 1, stream_identifier(identifier)),
        );
      } else {
        return new Error(new ProtocolError());
      }
    } else {
      return new Error(new ProtocolError());
    }
  } else {
    return new Error(new ProtocolError());
  }
}

function parse_priority(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if (length === 5) {
    if (
      $.bitSize >= 8 &&
      $.bitSize >= 9 &&
      $.bitSize >= 40 &&
      $.bitSize === 48 &&
      identifier !== 0
    ) {
      let exclusive = bitArraySliceToInt($, 8, 9, true, false);
      let dependency = bitArraySliceToInt($, 9, 40, true, false);
      let weight = $.byteAt(5);
      return new Ok(
        new Priority(
          exclusive === 1,
          stream_identifier(identifier),
          stream_identifier(dependency),
          weight,
        ),
      );
    } else {
      return new Error(new ProtocolError());
    }
  } else {
    return new Error(new FrameSizeError());
  }
}

function parse_push_promise(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if ($.bitSize >= 4 && $.bitSize >= 5 && $.bitSize >= 6 && $.bitSize >= 8) {
    let padded = bitArraySliceToInt($, 4, 5, true, false);
    if (
      $.bitSize >= 8 + padded * 8 &&
      $.bitSize >= 9 + padded * 8 &&
      $.bitSize >= 40 + padded * 8 &&
      length * 8 >= 0 &&
      $.bitSize >= 40 + length * 8 + padded * 8
    ) {
      let pad_length = bitArraySliceToInt($, 8, 8 + padded * 8, true, false);
      if (
        $.bitSize === 40 + length * 8 + pad_length * 8 + padded * 8 &&
        identifier !== 0
      ) {
        let padded$1 = padded;
        let end_headers = bitArraySliceToInt($, 5, 6, true, false);
        let pad_length$1 = pad_length;
        let promised_identifier = bitArraySliceToInt($, 9 + padded * 8, 9 + padded * 8 + 31, true, false);
        let data = bitArraySlice($, 40 + padded * 8, 40 + padded * 8 + length * 8);
        return new Ok(
          new PushPromise(
            (() => {
              let $1 = end_headers === 1;
              if ($1) {
                return new Complete(data);
              } else {
                return new Continued(data);
              }
            })(),
            stream_identifier(identifier),
            stream_identifier(promised_identifier),
          ),
        );
      } else {
        return new Error(new ProtocolError());
      }
    } else {
      return new Error(new ProtocolError());
    }
  } else {
    return new Error(new ProtocolError());
  }
}

function parse_ping(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if (length === 8) {
    if ($.bitSize >= 7 && $.bitSize >= 8 && $.bitSize === 72 && identifier === 0) {
      let ack = bitArraySliceToInt($, 7, 8, true, false);
      let data = bitArraySlice($, 8, 72);
      return new Ok(new Ping(ack === 1, data));
    } else {
      return new Error(new ProtocolError());
    }
  } else {
    return new Error(new FrameSizeError());
  }
}

function parse_window_update(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if (length === 4) {
    if ($.bitSize >= 8 && $.bitSize >= 9 && $.bitSize === 40) {
      let window_size = bitArraySliceToInt($, 9, 40, true, false);
      if (window_size !== 0) {
        return new Ok(
          new WindowUpdate(window_size, stream_identifier(identifier)),
        );
      } else {
        return new Error(new FrameSizeError());
      }
    } else {
      return new Error(new FrameSizeError());
    }
  } else {
    return new Error(new ProtocolError());
  }
}

function parse_continuation(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if (
    $.bitSize >= 5 &&
    $.bitSize >= 6 &&
    $.bitSize >= 8 &&
    length * 8 >= 0 &&
    $.bitSize === 8 + length * 8 &&
    identifier !== 0
  ) {
    let end_headers = bitArraySliceToInt($, 5, 6, true, false);
    let data = bitArraySlice($, 8, 8 + length * 8);
    return new Ok(
      new Continuation(
        (() => {
          let $1 = end_headers === 1;
          if ($1) {
            return new Complete(data);
          } else {
            return new Continued(data);
          }
        })(),
        stream_identifier(identifier),
      ),
    );
  } else {
    return new Error(new ProtocolError());
  }
}

function get_error(value) {
  if (value === 0) {
    return new NoError();
  } else if (value === 1) {
    return new ProtocolError();
  } else if (value === 2) {
    return new InternalError();
  } else if (value === 3) {
    return new FlowControlError();
  } else if (value === 4) {
    return new SettingsTimeout();
  } else if (value === 5) {
    return new StreamClosed();
  } else if (value === 6) {
    return new FrameSizeError();
  } else if (value === 7) {
    return new RefusedStream();
  } else if (value === 8) {
    return new Cancel();
  } else if (value === 9) {
    return new CompressionError();
  } else if (value === 10) {
    return new ConnectError();
  } else if (value === 11) {
    return new EnhanceYourCalm();
  } else if (value === 12) {
    return new InadequateSecurity();
  } else if (value === 13) {
    return new Http11Required();
  } else {
    let n = value;
    return new Unsupported(n);
  }
}

function parse_termination(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if (length === 4) {
    if ($.bitSize >= 8 && $.bitSize === 40 && identifier !== 0) {
      let error = bitArraySliceToInt($, 8, 40, true, false);
      return new Ok(
        new Termination(get_error(error), stream_identifier(identifier)),
      );
    } else {
      return new Error(new ProtocolError());
    }
  } else {
    return new Error(new FrameSizeError());
  }
}

function parse_go_away(identifier, flags, length, payload) {
  let $ = toBitArray([flags, payload]);
  if (
    $.bitSize >= 8 &&
    $.bitSize >= 9 &&
    $.bitSize >= 40 &&
    $.bitSize >= 72 &&
    length * 8 >= 0 &&
    $.bitSize === 72 + length * 8 &&
    identifier === 0
  ) {
    let last_stream_id = bitArraySliceToInt($, 9, 40, true, false);
    let error = bitArraySliceToInt($, 40, 72, true, false);
    let data = bitArraySlice($, 72, 72 + length * 8);
    return new Ok(
      new GoAway(data, get_error(error), stream_identifier(last_stream_id)),
    );
  } else {
    return new Error(new ProtocolError());
  }
}

function get_setting(identifier, value) {
  if (identifier === 1) {
    return new Ok(new HeaderTableSize(value));
  } else if (identifier === 2) {
    return new Ok(
      new ServerPush(
        (() => {
          if (value === 0) {
            return new Disabled();
          } else if (value === 1) {
            return new Enabled();
          } else {
            throw makeError(
              "panic",
              FILEPATH,
              "mist/internal/http2/frame",
              638,
              "get_setting",
              "Somehow a bit was neither 0 nor 1",
              {}
            )
          }
        })(),
      ),
    );
  } else if (identifier === 3) {
    return new Ok(new MaxConcurrentStreams(value));
  } else if (identifier === 4) {
    let n = value;
    if (n > 2_147_483_647) {
      return new Error(new FlowControlError());
    } else {
      return new Ok(new InitialWindowSize(value));
    }
  } else if (identifier === 5) {
    let n = value;
    if (n > 16_777_215) {
      return new Error(new ProtocolError());
    } else {
      return new Ok(new MaxFrameSize(value));
    }
  } else if (identifier === 6) {
    return new Ok(new MaxHeaderListSize(value));
  } else {
    return new Error(new ProtocolError());
  }
}

function get_settings(loop$data, loop$acc) {
  while (true) {
    let data = loop$data;
    let acc = loop$acc;
    if (data.bitSize === 0) {
      return new Ok(acc);
    } else if (data.bitSize >= 16 && data.bitSize >= 48) {
      let identifier = bitArraySliceToInt(data, 0, 16, true, false);
      let value = bitArraySliceToInt(data, 16, 48, true, false);
      let rest = bitArraySlice(data, 48);
      let $ = get_setting(identifier, value);
      if ($ instanceof Ok) {
        let setting = $[0];
        loop$data = rest;
        loop$acc = listPrepend(setting, acc);
      } else {
        return $;
      }
    } else {
      return new Error(new ProtocolError());
    }
  }
}

function parse_settings(identifier, flags, length, payload) {
  let $ = length % 6;
  let $1 = toBitArray([flags, payload]);
  if ($ === 0) {
    if (
      $1.bitSize >= 7 &&
      $1.bitSize >= 8 &&
      length * 8 >= 0 &&
      $1.bitSize === 8 + length * 8 &&
      identifier === 0
    ) {
      let ack = bitArraySliceToInt($1, 7, 8, true, false);
      let settings = bitArraySlice($1, 8, 8 + length * 8);
      return $result.try$(
        get_settings(settings, toList([])),
        (settings) => { return new Ok(new Settings(ack === 1, settings)); },
      );
    } else {
      return new Error(new ProtocolError());
    }
  } else {
    return new Error(new FrameSizeError());
  }
}

function from_bool(bool) {
  if (bool) {
    return 1;
  } else {
    return 0;
  }
}

function encode_priority(priority) {
  if (priority instanceof Some) {
    let exclusive = priority[0].exclusive;
    let weight = priority[0].weight;
    let dependency = priority[0].stream_dependency[0];
    let exclusive$1 = from_bool(exclusive);
    return toBitArray([
      sizedInt(exclusive$1, 1, true),
      sizedInt(dependency, 31, true),
      weight,
    ]);
  } else {
    return toBitArray([]);
  }
}

function encode_data(data) {
  if (data instanceof Complete) {
    let data$1 = data[0];
    return [1, data$1];
  } else {
    let data$1 = data[0];
    return [0, data$1];
  }
}

function encode_error(error) {
  if (error instanceof NoError) {
    return 0;
  } else if (error instanceof ProtocolError) {
    return 1;
  } else if (error instanceof InternalError) {
    return 2;
  } else if (error instanceof FlowControlError) {
    return 3;
  } else if (error instanceof SettingsTimeout) {
    return 4;
  } else if (error instanceof StreamClosed) {
    return 5;
  } else if (error instanceof FrameSizeError) {
    return 6;
  } else if (error instanceof RefusedStream) {
    return 7;
  } else if (error instanceof Cancel) {
    return 8;
  } else if (error instanceof CompressionError) {
    return 9;
  } else if (error instanceof ConnectError) {
    return 10;
  } else if (error instanceof EnhanceYourCalm) {
    return 11;
  } else if (error instanceof InadequateSecurity) {
    return 12;
  } else if (error instanceof Http11Required) {
    return 13;
  } else {
    return 69;
  }
}

function encode_settings(settings) {
  return $list.fold(
    settings,
    toBitArray([]),
    (acc, setting) => {
      if (setting instanceof HeaderTableSize) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([0, 1, sizedInt(value, 32, true)]),
        );
      } else if (setting instanceof ServerPush) {
        let $ = setting[0];
        if ($ instanceof Enabled) {
          return $bit_array.append(acc, toBitArray([0, 2, 0, 0, 0, 1]));
        } else {
          return $bit_array.append(acc, toBitArray([0, 2, 0, 0, 0, 0]));
        }
      } else if (setting instanceof MaxConcurrentStreams) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([0, 3, sizedInt(value, 32, true)]),
        );
      } else if (setting instanceof InitialWindowSize) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([0, 4, sizedInt(value, 32, true)]),
        );
      } else if (setting instanceof MaxFrameSize) {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([0, 5, sizedInt(value, 32, true)]),
        );
      } else {
        let value = setting[0];
        return $bit_array.append(
          acc,
          toBitArray([0, 6, sizedInt(value, 32, true)]),
        );
      }
    },
  );
}

export function encode(frame) {
  if (frame instanceof Data) {
    let data = frame.data;
    let end_stream = frame.end_stream;
    let identifier = frame.identifier[0];
    let length = $bit_array.byte_size(data);
    let end = from_bool(end_stream);
    return toBitArray([
      sizedInt(length, 24, true),
      0,
      sizedInt(0, 4, true),
      sizedInt(0, 1, true),
      sizedInt(0, 2, true),
      sizedInt(end, 1, true),
      sizedInt(0, 1, true),
      sizedInt(identifier, 31, true),
      data,
    ]);
  } else if (frame instanceof Header) {
    let data = frame.data;
    let end_stream = frame.end_stream;
    let priority = frame.priority;
    let identifier = frame.identifier[0];
    let $ = encode_data(data);
    let end_header;
    let data$1;
    end_header = $[0];
    data$1 = $[1];
    let length = $bit_array.byte_size(data$1);
    let end = from_bool(end_stream);
    let priority_flags = encode_priority(priority);
    let has_priority = from_bool($option.is_some(priority));
    return toBitArray([
      sizedInt(length, 24, true),
      1,
      sizedInt(0, 2, true),
      sizedInt(has_priority, 1, true),
      sizedInt(0, 1, true),
      sizedInt(0, 1, true),
      sizedInt(end_header, 1, true),
      sizedInt(0, 1, true),
      sizedInt(end, 1, true),
      sizedInt(0, 1, true),
      sizedInt(identifier, 31, true),
      priority_flags,
      data$1,
    ]);
  } else if (frame instanceof Priority) {
    let exclusive = frame.exclusive;
    let weight = frame.weight;
    let dependency = frame.stream_dependency[0];
    let identifier = frame.identifier[0];
    let exclusive$1 = from_bool(exclusive);
    return toBitArray([
      0, 0, 5,
      sizedInt(2, 2, true),
      0,
      sizedInt(0, 1, true),
      sizedInt(identifier, 31, true),
      sizedInt(exclusive$1, 1, true),
      sizedInt(dependency, 31, true),
      weight,
    ]);
  } else if (frame instanceof Termination) {
    let error = frame.error;
    let identifier = frame.identifier[0];
    let error_code = encode_error(error);
    return toBitArray([
      0, 0, 4,
      3,
      0,
      sizedInt(0, 1, true),
      sizedInt(identifier, 31, true),
      sizedInt(error_code, 32, true),
    ]);
  } else if (frame instanceof Settings) {
    let ack = frame.ack;
    let settings = frame.settings;
    let ack$1 = from_bool(ack);
    let settings$1 = encode_settings(settings);
    let length = $bit_array.byte_size(settings$1);
    return toBitArray([
      sizedInt(length, 24, true),
      4,
      sizedInt(0, 7, true),
      sizedInt(ack$1, 1, true),
      sizedInt(0, 1, true),
      sizedInt(0, 31, true),
      settings$1,
    ]);
  } else if (frame instanceof PushPromise) {
    let data = frame.data;
    let promised_identifier = frame.promised_stream_id[0];
    let identifier = frame.identifier[0];
    let $ = encode_data(data);
    let end_headers;
    let data$1;
    end_headers = $[0];
    data$1 = $[1];
    return toBitArray([
      0, 0, 0,
      5,
      sizedInt(0, 4, true),
      ,
      sizedInt(end_headers, 1, true),
      sizedInt(0, 2, true),
      sizedInt(0, 1, true),
      sizedInt(identifier, 31, true),
      sizedInt(0, 1, true),
      sizedInt(promised_identifier, 31, true),
      data$1,
    ]);
  } else if (frame instanceof Ping) {
    let ack = frame.ack;
    let data = frame.data;
    let ack$1 = from_bool(ack);
    return toBitArray([
      0, 0, 0,
      6,
      sizedInt(0, 7, true),
      sizedInt(ack$1, 1, true),
      sizedInt(0, 1, true),
      sizedInt(0, 31, true),
      data,
    ]);
  } else if (frame instanceof GoAway) {
    let data = frame.data;
    let error = frame.error;
    let last_stream_id = frame.last_stream_id[0];
    let error$1 = encode_error(error);
    let payload_size = $bit_array.byte_size(data);
    return toBitArray([
      sizedInt(payload_size, 24, true),
      7,
      0,
      sizedInt(0, 1, true),
      sizedInt(0, 31, true),
      sizedInt(0, 1, true),
      sizedInt(last_stream_id, 31, true),
      sizedInt(error$1, 32, true),
      data,
    ]);
  } else if (frame instanceof WindowUpdate) {
    let amount = frame.amount;
    let identifier = frame.identifier[0];
    return toBitArray([
      0, 0, 4,
      8,
      0,
      sizedInt(0, 1, true),
      sizedInt(identifier, 31, true),
      sizedInt(0, 1, true),
      sizedInt(amount, 31, true),
    ]);
  } else {
    let data = frame.data;
    let identifier = frame.identifier[0];
    let $ = encode_data(data);
    let end_headers;
    let data$1;
    end_headers = $[0];
    data$1 = $[1];
    let payload_size = $bit_array.byte_size(data$1);
    return toBitArray([
      sizedInt(payload_size, 24, true),
      9,
      sizedInt(0, 5, true),
      sizedInt(end_headers, 1, true),
      sizedInt(0, 2, true),
      sizedInt(0, 1, true),
      sizedInt(identifier, 31, true),
      data$1,
    ]);
  }
}

export function settings_ack() {
  return new Settings(true, toList([]));
}
