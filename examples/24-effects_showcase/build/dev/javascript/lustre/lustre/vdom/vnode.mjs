import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import * as $string_tree from "../../../gleam_stdlib/gleam/string_tree.mjs";
import * as $houdini from "../../../houdini/houdini.mjs";
import {
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../../gleam.mjs";
import * as $json_object_builder from "../../lustre/internals/json_object_builder.mjs";
import * as $mutable_map from "../../lustre/internals/mutable_map.mjs";
import * as $vattr from "../../lustre/vdom/vattr.mjs";

export class Fragment extends $CustomType {
  constructor(kind, key, mapper, children, keyed_children) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.children = children;
    this.keyed_children = keyed_children;
  }
}

export class Element extends $CustomType {
  constructor(kind, key, mapper, namespace, tag, attributes, children, keyed_children, self_closing, void$) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.children = children;
    this.keyed_children = keyed_children;
    this.self_closing = self_closing;
    this.void = void$;
  }
}

export class Text extends $CustomType {
  constructor(kind, key, mapper, content) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.content = content;
  }
}

export class UnsafeInnerHtml extends $CustomType {
  constructor(kind, key, mapper, namespace, tag, attributes, inner_html) {
    super();
    this.kind = kind;
    this.key = key;
    this.mapper = mapper;
    this.namespace = namespace;
    this.tag = tag;
    this.attributes = attributes;
    this.inner_html = inner_html;
  }
}

function is_void_element(tag, namespace) {
  if (namespace === "") {
    if (tag === "area") {
      return true;
    } else if (tag === "base") {
      return true;
    } else if (tag === "br") {
      return true;
    } else if (tag === "col") {
      return true;
    } else if (tag === "embed") {
      return true;
    } else if (tag === "hr") {
      return true;
    } else if (tag === "img") {
      return true;
    } else if (tag === "input") {
      return true;
    } else if (tag === "link") {
      return true;
    } else if (tag === "meta") {
      return true;
    } else if (tag === "param") {
      return true;
    } else if (tag === "source") {
      return true;
    } else if (tag === "track") {
      return true;
    } else if (tag === "wbr") {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}

export function to_keyed(key, node) {
  if (node instanceof Fragment) {
    return new Fragment(
      node.kind,
      key,
      node.mapper,
      node.children,
      node.keyed_children,
    );
  } else if (node instanceof Element) {
    return new Element(
      node.kind,
      key,
      node.mapper,
      node.namespace,
      node.tag,
      node.attributes,
      node.children,
      node.keyed_children,
      node.self_closing,
      node.void,
    );
  } else if (node instanceof Text) {
    return new Text(node.kind, key, node.mapper, node.content);
  } else {
    return new UnsafeInnerHtml(
      node.kind,
      key,
      node.mapper,
      node.namespace,
      node.tag,
      node.attributes,
      node.inner_html,
    );
  }
}

function text_to_json(kind, key, content) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.string(_pipe$1, "content", content);
  return $json_object_builder.build(_pipe$2);
}

function unsafe_inner_html_to_json(
  kind,
  key,
  namespace,
  tag,
  attributes,
  inner_html
) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.string(_pipe$1, "namespace", namespace);
  let _pipe$3 = $json_object_builder.string(_pipe$2, "tag", tag);
  let _pipe$4 = $json_object_builder.list(
    _pipe$3,
    "attributes",
    attributes,
    $vattr.to_json,
  );
  let _pipe$5 = $json_object_builder.string(_pipe$4, "inner_html", inner_html);
  return $json_object_builder.build(_pipe$5);
}

export const fragment_kind = 0;

export function fragment(key, mapper, children, keyed_children) {
  return new Fragment(fragment_kind, key, mapper, children, keyed_children);
}

export const element_kind = 1;

export function element(
  key,
  mapper,
  namespace,
  tag,
  attributes,
  children,
  keyed_children,
  self_closing,
  void$
) {
  return new Element(
    element_kind,
    key,
    mapper,
    namespace,
    tag,
    $vattr.prepare(attributes),
    children,
    keyed_children,
    self_closing,
    void$ || is_void_element(tag, namespace),
  );
}

export const text_kind = 2;

export function text(key, mapper, content) {
  return new Text(text_kind, key, mapper, content);
}

export const unsafe_inner_html_kind = 3;

export function unsafe_inner_html(
  key,
  mapper,
  namespace,
  tag,
  attributes,
  inner_html
) {
  return new UnsafeInnerHtml(
    unsafe_inner_html_kind,
    key,
    mapper,
    namespace,
    tag,
    $vattr.prepare(attributes),
    inner_html,
  );
}

function children_to_snapshot_builder(
  loop$html,
  loop$children,
  loop$raw_text,
  loop$indent
) {
  while (true) {
    let html = loop$html;
    let children = loop$children;
    let raw_text = loop$raw_text;
    let indent = loop$indent;
    if (children instanceof $Empty) {
      return html;
    } else {
      let $ = children.head;
      if ($ instanceof Fragment) {
        let child = $;
        let rest = children.tail;
        let _pipe = child;
        let _pipe$1 = do_to_snapshot_builder(_pipe, raw_text, indent);
        let _pipe$2 = ((_capture) => {
          return $string_tree.append_tree(html, _capture);
        })(_pipe$1);
        loop$html = _pipe$2;
        loop$children = rest;
        loop$raw_text = raw_text;
        loop$indent = indent;
      } else if ($ instanceof Text) {
        let $1 = children.tail;
        if ($1 instanceof $Empty) {
          let child = $;
          let rest = $1;
          let _pipe = child;
          let _pipe$1 = do_to_snapshot_builder(_pipe, raw_text, indent);
          let _pipe$2 = $string_tree.append(_pipe$1, "\n");
          let _pipe$3 = ((_capture) => {
            return $string_tree.append_tree(html, _capture);
          })(_pipe$2);
          loop$html = _pipe$3;
          loop$children = rest;
          loop$raw_text = raw_text;
          loop$indent = indent;
        } else {
          let $2 = $1.head;
          if ($2 instanceof Text) {
            let a = $.content;
            let rest = $1.tail;
            let b = $2.content;
            loop$html = html;
            loop$children = listPrepend(
              new Text(text_kind, "", $function.identity, a + b),
              rest,
            );
            loop$raw_text = raw_text;
            loop$indent = indent;
          } else {
            let child = $;
            let rest = $1;
            let _pipe = child;
            let _pipe$1 = do_to_snapshot_builder(_pipe, raw_text, indent);
            let _pipe$2 = $string_tree.append(_pipe$1, "\n");
            let _pipe$3 = ((_capture) => {
              return $string_tree.append_tree(html, _capture);
            })(_pipe$2);
            loop$html = _pipe$3;
            loop$children = rest;
            loop$raw_text = raw_text;
            loop$indent = indent;
          }
        }
      } else {
        let child = $;
        let rest = children.tail;
        let _pipe = child;
        let _pipe$1 = do_to_snapshot_builder(_pipe, raw_text, indent);
        let _pipe$2 = $string_tree.append(_pipe$1, "\n");
        let _pipe$3 = ((_capture) => {
          return $string_tree.append_tree(html, _capture);
        })(_pipe$2);
        loop$html = _pipe$3;
        loop$children = rest;
        loop$raw_text = raw_text;
        loop$indent = indent;
      }
    }
  }
}

function do_to_snapshot_builder(node, raw_text, indent) {
  let spaces = $string.repeat("  ", indent);
  if (node instanceof Fragment) {
    let $ = node.children;
    if ($ instanceof $Empty) {
      return $string_tree.new$();
    } else {
      let children = $;
      let _pipe = $string_tree.new$();
      return children_to_snapshot_builder(_pipe, children, raw_text, indent);
    }
  } else if (node instanceof Element) {
    let self_closing = node.self_closing;
    if (self_closing) {
      let key = node.key;
      let namespace = node.namespace;
      let tag = node.tag;
      let attributes = node.attributes;
      let html = $string_tree.from_string("<" + tag);
      let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
      let _pipe = html;
      let _pipe$1 = $string_tree.prepend(_pipe, spaces);
      let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
      return $string_tree.append(_pipe$2, "/>");
    } else {
      let void$ = node.void;
      if (void$) {
        let key = node.key;
        let namespace = node.namespace;
        let tag = node.tag;
        let attributes = node.attributes;
        let html = $string_tree.from_string("<" + tag);
        let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
        let _pipe = html;
        let _pipe$1 = $string_tree.prepend(_pipe, spaces);
        let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
        return $string_tree.append(_pipe$2, ">");
      } else {
        let $ = node.children;
        if ($ instanceof $Empty) {
          let key = node.key;
          let namespace = node.namespace;
          let tag = node.tag;
          let attributes = node.attributes;
          let html = $string_tree.from_string("<" + tag);
          let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
          let _pipe = html;
          let _pipe$1 = $string_tree.prepend(_pipe, spaces);
          let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
          let _pipe$3 = $string_tree.append(_pipe$2, ">");
          return $string_tree.append(_pipe$3, ("</" + tag) + ">");
        } else {
          let key = node.key;
          let namespace = node.namespace;
          let tag = node.tag;
          let attributes = node.attributes;
          let children = $;
          let html = $string_tree.from_string("<" + tag);
          let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
          let _pipe = html;
          let _pipe$1 = $string_tree.prepend(_pipe, spaces);
          let _pipe$2 = $string_tree.append_tree(_pipe$1, attributes$1);
          let _pipe$3 = $string_tree.append(_pipe$2, ">\n");
          let _pipe$4 = children_to_snapshot_builder(
            _pipe$3,
            children,
            raw_text,
            indent + 1,
          );
          let _pipe$5 = $string_tree.append(_pipe$4, spaces);
          return $string_tree.append(_pipe$5, ("</" + tag) + ">");
        }
      }
    }
  } else if (node instanceof Text) {
    let $ = node.content;
    if ($ === "") {
      return $string_tree.new$();
    } else if (raw_text) {
      let content = $;
      return $string_tree.from_strings(toList([spaces, content]));
    } else {
      let content = $;
      return $string_tree.from_strings(
        toList([spaces, $houdini.escape(content)]),
      );
    }
  } else {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let inner_html = node.inner_html;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
    let _pipe$2 = $string_tree.append(_pipe$1, ">");
    let _pipe$3 = $string_tree.append(_pipe$2, inner_html);
    return $string_tree.append(_pipe$3, ("</" + tag) + ">");
  }
}

export function to_snapshot(node) {
  let _pipe = node;
  let _pipe$1 = do_to_snapshot_builder(_pipe, false, 0);
  return $string_tree.to_string(_pipe$1);
}

function children_to_string_tree(html, children) {
  return $list.fold(
    children,
    html,
    (html, child) => {
      let _pipe = child;
      let _pipe$1 = to_string_tree(_pipe);
      return ((_capture) => { return $string_tree.append_tree(html, _capture); })(
        _pipe$1,
      );
    },
  );
}

export function to_string_tree(node) {
  if (node instanceof Fragment) {
    let children = node.children;
    return children_to_string_tree($string_tree.new$(), children);
  } else if (node instanceof Element) {
    let self_closing = node.self_closing;
    if (self_closing) {
      let key = node.key;
      let namespace = node.namespace;
      let tag = node.tag;
      let attributes = node.attributes;
      let html = $string_tree.from_string("<" + tag);
      let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
      let _pipe = html;
      let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
      return $string_tree.append(_pipe$1, "/>");
    } else {
      let void$ = node.void;
      if (void$) {
        let key = node.key;
        let namespace = node.namespace;
        let tag = node.tag;
        let attributes = node.attributes;
        let html = $string_tree.from_string("<" + tag);
        let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
        let _pipe = html;
        let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
        return $string_tree.append(_pipe$1, ">");
      } else {
        let key = node.key;
        let namespace = node.namespace;
        let tag = node.tag;
        let attributes = node.attributes;
        let children = node.children;
        let html = $string_tree.from_string("<" + tag);
        let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
        let _pipe = html;
        let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
        let _pipe$2 = $string_tree.append(_pipe$1, ">");
        let _pipe$3 = children_to_string_tree(_pipe$2, children);
        return $string_tree.append(_pipe$3, ("</" + tag) + ">");
      }
    }
  } else if (node instanceof Text) {
    let $ = node.content;
    if ($ === "") {
      return $string_tree.new$();
    } else {
      let content = $;
      return $string_tree.from_string($houdini.escape(content));
    }
  } else {
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let inner_html = node.inner_html;
    let html = $string_tree.from_string("<" + tag);
    let attributes$1 = $vattr.to_string_tree(key, namespace, attributes);
    let _pipe = html;
    let _pipe$1 = $string_tree.append_tree(_pipe, attributes$1);
    let _pipe$2 = $string_tree.append(_pipe$1, ">");
    let _pipe$3 = $string_tree.append(_pipe$2, inner_html);
    return $string_tree.append(_pipe$3, ("</" + tag) + ">");
  }
}

export function to_string(node) {
  let _pipe = node;
  let _pipe$1 = to_string_tree(_pipe);
  return $string_tree.to_string(_pipe$1);
}

function fragment_to_json(kind, key, children) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.list(
    _pipe$1,
    "children",
    children,
    to_json,
  );
  return $json_object_builder.build(_pipe$2);
}

export function to_json(node) {
  if (node instanceof Fragment) {
    let kind = node.kind;
    let key = node.key;
    let children = node.children;
    return fragment_to_json(kind, key, children);
  } else if (node instanceof Element) {
    let kind = node.kind;
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let children = node.children;
    return element_to_json(kind, key, namespace, tag, attributes, children);
  } else if (node instanceof Text) {
    let kind = node.kind;
    let key = node.key;
    let content = node.content;
    return text_to_json(kind, key, content);
  } else {
    let kind = node.kind;
    let key = node.key;
    let namespace = node.namespace;
    let tag = node.tag;
    let attributes = node.attributes;
    let inner_html = node.inner_html;
    return unsafe_inner_html_to_json(
      kind,
      key,
      namespace,
      tag,
      attributes,
      inner_html,
    );
  }
}

function element_to_json(kind, key, namespace, tag, attributes, children) {
  let _pipe = $json_object_builder.tagged(kind);
  let _pipe$1 = $json_object_builder.string(_pipe, "key", key);
  let _pipe$2 = $json_object_builder.string(_pipe$1, "namespace", namespace);
  let _pipe$3 = $json_object_builder.string(_pipe$2, "tag", tag);
  let _pipe$4 = $json_object_builder.list(
    _pipe$3,
    "attributes",
    attributes,
    $vattr.to_json,
  );
  let _pipe$5 = $json_object_builder.list(
    _pipe$4,
    "children",
    children,
    to_json,
  );
  return $json_object_builder.build(_pipe$5);
}
