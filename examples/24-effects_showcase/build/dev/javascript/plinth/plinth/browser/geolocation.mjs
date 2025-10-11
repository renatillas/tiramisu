import * as $promise from "../../../gleam_javascript/gleam/javascript/promise.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { getCurrentPosition as get_current_position } from "../../geolocation_ffi.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";

export { get_current_position };

export class GeolocationPosition extends $CustomType {
  constructor(latitude, longitude, altitude, accuracy, altitude_accuracy, heading, speed, timestamp) {
    super();
    this.latitude = latitude;
    this.longitude = longitude;
    this.altitude = altitude;
    this.accuracy = accuracy;
    this.altitude_accuracy = altitude_accuracy;
    this.heading = heading;
    this.speed = speed;
    this.timestamp = timestamp;
  }
}

export function decoder() {
  return $decode.field(
    "timestamp",
    $decode.float,
    (timestamp) => {
      return $decode.field(
        "coords",
        $decode.field(
          "latitude",
          $decode.float,
          (latitude) => {
            return $decode.field(
              "longitude",
              $decode.float,
              (longitude) => {
                return $decode.field(
                  "altitude",
                  $decode.optional($decode.float),
                  (altitude) => {
                    return $decode.field(
                      "accuracy",
                      $decode.float,
                      (accuracy) => {
                        return $decode.field(
                          "altitudeAccuracy",
                          $decode.optional($decode.float),
                          (altitude_accuracy) => {
                            return $decode.field(
                              "heading",
                              $decode.optional($decode.float),
                              (heading) => {
                                return $decode.field(
                                  "speed",
                                  $decode.optional($decode.float),
                                  (speed) => {
                                    return $decode.success(
                                      new GeolocationPosition(
                                        latitude,
                                        longitude,
                                        altitude,
                                        accuracy,
                                        altitude_accuracy,
                                        heading,
                                        speed,
                                        timestamp,
                                      ),
                                    );
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
              },
            );
          },
        ),
        (n) => { return $decode.success(n); },
      );
    },
  );
}

export function current_position() {
  return $promise.new$(
    (resolve) => {
      return get_current_position(
        (position) => {
          let _block;
          let $ = $decode.run(position, decoder());
          if ($ instanceof Ok) {
            _block = $;
          } else {
            let reason = $[0];
            _block = new Error($string.inspect(reason));
          }
          let _pipe = _block;
          return resolve(_pipe);
        },
        (error) => { return resolve(new Error($string.inspect(error))); },
      );
    },
  );
}
