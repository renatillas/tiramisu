import * as $filepath from "../../../filepath/filepath.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { attribute } from "../../../lustre/lustre/attribute.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import * as $tom from "../../../tom/tom.mjs";
import { Ok, Error, toList, Empty as $Empty, prepend as listPrepend } from "../../gleam.mjs";
import * as $project from "../../lustre_dev_tools/project.mjs";

function lang(project) {
  return $attribute.lang(
    (() => {
      let _pipe = $tom.get_string(project.options, toList(["html", "lang"]));
      return $result.unwrap(_pipe, "en");
    })(),
  );
}

function title(project, entry) {
  let name = $filepath.base_name(entry);
  return $html.title(
    toList([]),
    (() => {
      let _pipe = $tom.get_string(project.options, toList(["html", "title"]));
      return $result.unwrap(_pipe, name);
    })(),
  );
}

function stylesheets(project) {
  return $element.fragment(
    (() => {
      let _pipe = $tom.get_array(
        project.options,
        toList(["html", "stylesheets"]),
      );
      let _pipe$1 = $result.map(
        _pipe,
        (_capture) => { return $list.filter_map(_capture, $tom.as_table); },
      );
      let _pipe$2 = $result.map(
        _pipe$1,
        (_capture) => {
          return $list.filter_map(
            _capture,
            (toml) => {
              let href = $tom.get_string(toml, toList(["href"]));
              let content = $tom.get_string(toml, toList(["content"]));
              if (href instanceof Ok) {
                let href$1 = href[0];
                return new Ok(
                  $html.link(
                    toList([
                      $attribute.href(href$1),
                      $attribute.rel("stylesheet"),
                    ]),
                  ),
                );
              } else if (content instanceof Ok) {
                let content$1 = content[0];
                return new Ok($html.style(toList([]), content$1));
              } else {
                return new Error(undefined);
              }
            },
          );
        },
      );
      return $result.unwrap(_pipe$2, toList([]));
    })(),
  );
}

function scripts(project) {
  return $element.fragment(
    (() => {
      let _pipe = $tom.get_array(project.options, toList(["html", "scripts"]));
      let _pipe$1 = $result.map(
        _pipe,
        (_capture) => { return $list.filter_map(_capture, $tom.as_table); },
      );
      let _pipe$2 = $result.map(
        _pipe$1,
        (_capture) => {
          return $list.filter_map(
            _capture,
            (toml) => {
              let src = $tom.get_string(toml, toList(["src"]));
              let content = $tom.get_string(toml, toList(["content"]));
              let type_ = $tom.get_string(toml, toList(["type"]));
              if (type_ instanceof Ok) {
                if (src instanceof Ok) {
                  let type_value = type_[0];
                  let src$1 = src[0];
                  return new Ok(
                    $html.script(
                      toList([
                        $attribute.src(src$1),
                        $attribute.type_(type_value),
                      ]),
                      "",
                    ),
                  );
                } else if (content instanceof Ok) {
                  let type_value = type_[0];
                  let content$1 = content[0];
                  return new Ok(
                    $html.script(
                      toList([$attribute.type_(type_value)]),
                      content$1,
                    ),
                  );
                } else {
                  return new Error(undefined);
                }
              } else if (src instanceof Ok) {
                let src$1 = src[0];
                return new Ok($html.script(toList([$attribute.src(src$1)]), ""));
              } else if (content instanceof Ok) {
                let content$1 = content[0];
                return new Ok($html.script(toList([]), content$1));
              } else {
                return new Error(undefined);
              }
            },
          );
        },
      );
      return $result.unwrap(_pipe$2, toList([]));
    })(),
  );
}

function as_attribute(key, toml) {
  let $ = $tom.as_string(toml);
  let $1 = $tom.as_bool(toml);
  if ($ instanceof Ok) {
    let value = $[0];
    return new Ok(attribute(key, value));
  } else if ($1 instanceof Ok) {
    let $2 = $1[0];
    if ($2) {
      return new Ok(attribute(key, ""));
    } else {
      return new Error(undefined);
    }
  } else {
    return new Error(undefined);
  }
}

function meta(project) {
  return $element.fragment(
    (() => {
      let _pipe = $tom.get_array(project.options, toList(["html", "meta"]));
      let _pipe$1 = $result.map(
        _pipe,
        (_capture) => { return $list.filter_map(_capture, $tom.as_table); },
      );
      let _pipe$2 = $result.map(
        _pipe$1,
        (_capture) => {
          return $list.map(
            _capture,
            ((_capture) => {
              return $dict.fold(
                _capture,
                toList([]),
                (attributes, key, toml) => {
                  let $ = as_attribute(key, toml);
                  if ($ instanceof Ok) {
                    let attribute$1 = $[0];
                    return listPrepend(attribute$1, attributes);
                  } else {
                    return attributes;
                  }
                },
              );
            }),
          );
        },
      );
      let _pipe$3 = $result.map(
        _pipe$2,
        (_capture) => {
          return $list.filter_map(
            _capture,
            (attributes) => {
              if (attributes instanceof $Empty) {
                return new Error(undefined);
              } else {
                return new Ok($html.meta(attributes));
              }
            },
          );
        },
      );
      return $result.unwrap(_pipe$3, toList([]));
    })(),
  );
}

function links(project) {
  return $element.fragment(
    (() => {
      let _pipe = $tom.get_array(project.options, toList(["html", "links"]));
      let _pipe$1 = $result.map(
        _pipe,
        (_capture) => { return $list.filter_map(_capture, $tom.as_table); },
      );
      let _pipe$2 = $result.map(
        _pipe$1,
        (_capture) => {
          return $list.map(
            _capture,
            ((_capture) => {
              return $dict.fold(
                _capture,
                toList([]),
                (attributes, key, toml) => {
                  let $ = as_attribute(key, toml);
                  if ($ instanceof Ok) {
                    let attribute$1 = $[0];
                    return listPrepend(attribute$1, attributes);
                  } else {
                    return attributes;
                  }
                },
              );
            }),
          );
        },
      );
      let _pipe$3 = $result.map(
        _pipe$2,
        (_capture) => {
          return $list.filter_map(
            _capture,
            (attributes) => {
              if (attributes instanceof $Empty) {
                return new Error(undefined);
              } else {
                return new Ok($html.link(attributes));
              }
            },
          );
        },
      );
      return $result.unwrap(_pipe$3, toList([]));
    })(),
  );
}

function body(project) {
  return $element.unsafe_raw_html(
    "",
    "body",
    toList([]),
    (() => {
      let _pipe = $tom.get_string(project.options, toList(["html", "body"]));
      return $result.unwrap(_pipe, "<div id=\"app\"></div>");
    })(),
  );
}

/**
 *
 */
export function generate(project, entry, tailwind_entry, minify) {
  let name = $filepath.base_name(entry);
  let html = $html.html(
    toList([lang(project)]),
    toList([
      $html.head(
        toList([]),
        toList([
          $html.meta(toList([$attribute.charset("utf-8")])),
          $html.meta(
            toList([
              $attribute.name("viewport"),
              $attribute.content("width=device-width, initial-scale=1"),
            ]),
          ),
          meta(project),
          title(project, entry),
          links(project),
          stylesheets(project),
          (() => {
            if (tailwind_entry instanceof Some) {
              let entry$1 = tailwind_entry[0];
              return $html.link(
                toList([
                  $attribute.rel("stylesheet"),
                  $attribute.href(("/" + $filepath.base_name(entry$1)) + ".css"),
                ]),
              );
            } else {
              return $element.none();
            }
          })(),
          scripts(project),
          $html.script(
            toList([
              $attribute.type_("module"),
              $attribute.src(("/" + name) + ".js"),
            ]),
            "",
          ),
        ]),
      ),
      body(project),
    ]),
  );
  if (minify) {
    return $element.to_document_string(html);
  } else {
    return "<!doctype html>\n" + $element.to_readable_string(html);
  }
}

/**
 *
 */
export function dev(project, entry, tailwind_entry) {
  let html = $html.html(
    toList([lang(project)]),
    toList([
      $html.head(
        toList([]),
        toList([
          $html.meta(toList([$attribute.charset("utf-8")])),
          $html.meta(
            toList([
              $attribute.name("viewport"),
              $attribute.content("width=device-width, initial-scale=1"),
            ]),
          ),
          meta(project),
          title(project, entry),
          links(project),
          stylesheets(project),
          (() => {
            if (tailwind_entry instanceof Some) {
              let entry$1 = tailwind_entry[0];
              return $html.link(
                toList([
                  $attribute.rel("stylesheet"),
                  $attribute.href(("/" + entry$1) + ".css"),
                ]),
              );
            } else {
              return $element.none();
            }
          })(),
          scripts(project),
          $html.script(
            toList([$attribute.src("/.lustre/server-hot-reload.js")]),
            "",
          ),
          (() => {
            let $ = project.has_node_modules;
            if ($) {
              return $html.script(
                toList([
                  $attribute.type_("module"),
                  $attribute.src(entry + ".dev.js"),
                ]),
                "",
              );
            } else {
              return $html.script(
                toList([$attribute.type_("module")]),
                (() => {
                  let _pipe = "\n              import { main } from '/${name}/${entry}.mjs';\n\n              main();\n              ";
                  let _pipe$1 = $string.replace(_pipe, "${name}", project.name);
                  return $string.replace(_pipe$1, "${entry}", entry);
                })(),
              );
            }
          })(),
        ]),
      ),
      body(project),
    ]),
  );
  return $element.to_document_string(html);
}
