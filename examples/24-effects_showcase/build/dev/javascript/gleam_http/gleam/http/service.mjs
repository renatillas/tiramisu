import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error } from "../../gleam.mjs";
import * as $http from "../../gleam/http.mjs";
import { Delete, Patch, Post, Put } from "../../gleam/http.mjs";
import * as $request from "../../gleam/http/request.mjs";
import * as $response from "../../gleam/http/response.mjs";

export function map_response_body(service, mapper) {
  return (req) => {
    let _pipe = req;
    let _pipe$1 = service(_pipe);
    return $response.map(_pipe$1, mapper);
  };
}

export function prepend_response_header(service, key, value) {
  return (req) => {
    let _pipe = req;
    let _pipe$1 = service(_pipe);
    return $response.prepend_header(_pipe$1, key, value);
  };
}

function ensure_post(req) {
  let $ = req.method;
  if ($ instanceof Post) {
    return new Ok(req);
  } else {
    return new Error(undefined);
  }
}

function get_override_method(request) {
  return $result.try$(
    $request.get_query(request),
    (query_params) => {
      return $result.try$(
        $list.key_find(query_params, "_method"),
        (method) => {
          return $result.try$(
            $http.parse_method(method),
            (method) => {
              if (method instanceof Put) {
                return new Ok(method);
              } else if (method instanceof Delete) {
                return new Ok(method);
              } else if (method instanceof Patch) {
                return new Ok(method);
              } else {
                return new Error(undefined);
              }
            },
          );
        },
      );
    },
  );
}

export function method_override(service) {
  return (request) => {
    let _pipe = request;
    let _pipe$1 = ensure_post(_pipe);
    let _pipe$2 = $result.try$(_pipe$1, get_override_method);
    let _pipe$3 = $result.map(
      _pipe$2,
      (_capture) => { return $request.set_method(request, _capture); },
    );
    let _pipe$4 = $result.unwrap(_pipe$3, request);
    return service(_pipe$4);
  };
}
