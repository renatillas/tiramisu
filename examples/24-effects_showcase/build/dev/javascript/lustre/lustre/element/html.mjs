import * as $json from "../../../gleam_json/gleam/json.mjs";
import { toList, prepend as listPrepend } from "../../gleam.mjs";
import * as $attribute from "../../lustre/attribute.mjs";
import * as $element from "../../lustre/element.mjs";
import { element, namespaced } from "../../lustre/element.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";

/**
 *
 */
export function html(attrs, children) {
  return element("html", attrs, children);
}

export function text(content) {
  return $element.text(content);
}

/**
 *
 */
export function base(attrs) {
  return element("base", attrs, $constants.empty_list);
}

/**
 *
 */
export function head(attrs, children) {
  return element("head", attrs, children);
}

/**
 *
 */
export function link(attrs) {
  return element("link", attrs, $constants.empty_list);
}

/**
 *
 */
export function meta(attrs) {
  return element("meta", attrs, $constants.empty_list);
}

/**
 *
 */
export function style(attrs, css) {
  return $element.unsafe_raw_html("", "style", attrs, css);
}

/**
 *
 */
export function title(attrs, content) {
  return element("title", attrs, toList([text(content)]));
}

/**
 *
 */
export function body(attrs, children) {
  return element("body", attrs, children);
}

/**
 *
 */
export function address(attrs, children) {
  return element("address", attrs, children);
}

/**
 *
 */
export function article(attrs, children) {
  return element("article", attrs, children);
}

/**
 *
 */
export function aside(attrs, children) {
  return element("aside", attrs, children);
}

/**
 *
 */
export function footer(attrs, children) {
  return element("footer", attrs, children);
}

/**
 *
 */
export function header(attrs, children) {
  return element("header", attrs, children);
}

/**
 *
 */
export function h1(attrs, children) {
  return element("h1", attrs, children);
}

/**
 *
 */
export function h2(attrs, children) {
  return element("h2", attrs, children);
}

/**
 *
 */
export function h3(attrs, children) {
  return element("h3", attrs, children);
}

/**
 *
 */
export function h4(attrs, children) {
  return element("h4", attrs, children);
}

/**
 *
 */
export function h5(attrs, children) {
  return element("h5", attrs, children);
}

/**
 *
 */
export function h6(attrs, children) {
  return element("h6", attrs, children);
}

/**
 *
 */
export function hgroup(attrs, children) {
  return element("hgroup", attrs, children);
}

/**
 *
 */
export function main(attrs, children) {
  return element("main", attrs, children);
}

/**
 *
 */
export function nav(attrs, children) {
  return element("nav", attrs, children);
}

/**
 *
 */
export function section(attrs, children) {
  return element("section", attrs, children);
}

/**
 *
 */
export function search(attrs, children) {
  return element("search", attrs, children);
}

/**
 *
 */
export function blockquote(attrs, children) {
  return element("blockquote", attrs, children);
}

/**
 *
 */
export function dd(attrs, children) {
  return element("dd", attrs, children);
}

/**
 *
 */
export function div(attrs, children) {
  return element("div", attrs, children);
}

/**
 *
 */
export function dl(attrs, children) {
  return element("dl", attrs, children);
}

/**
 *
 */
export function dt(attrs, children) {
  return element("dt", attrs, children);
}

/**
 *
 */
export function figcaption(attrs, children) {
  return element("figcaption", attrs, children);
}

/**
 *
 */
export function figure(attrs, children) {
  return element("figure", attrs, children);
}

/**
 *
 */
export function hr(attrs) {
  return element("hr", attrs, $constants.empty_list);
}

/**
 *
 */
export function li(attrs, children) {
  return element("li", attrs, children);
}

/**
 *
 */
export function menu(attrs, children) {
  return element("menu", attrs, children);
}

/**
 *
 */
export function ol(attrs, children) {
  return element("ol", attrs, children);
}

/**
 *
 */
export function p(attrs, children) {
  return element("p", attrs, children);
}

/**
 *
 */
export function pre(attrs, children) {
  return element("pre", attrs, children);
}

/**
 *
 */
export function ul(attrs, children) {
  return element("ul", attrs, children);
}

/**
 *
 */
export function a(attrs, children) {
  return element("a", attrs, children);
}

/**
 *
 */
export function abbr(attrs, children) {
  return element("abbr", attrs, children);
}

/**
 *
 */
export function b(attrs, children) {
  return element("b", attrs, children);
}

/**
 *
 */
export function bdi(attrs, children) {
  return element("bdi", attrs, children);
}

/**
 *
 */
export function bdo(attrs, children) {
  return element("bdo", attrs, children);
}

/**
 *
 */
export function br(attrs) {
  return element("br", attrs, $constants.empty_list);
}

/**
 *
 */
export function cite(attrs, children) {
  return element("cite", attrs, children);
}

/**
 *
 */
export function code(attrs, children) {
  return element("code", attrs, children);
}

/**
 *
 */
export function data(attrs, children) {
  return element("data", attrs, children);
}

/**
 *
 */
export function dfn(attrs, children) {
  return element("dfn", attrs, children);
}

/**
 *
 */
export function em(attrs, children) {
  return element("em", attrs, children);
}

/**
 *
 */
export function i(attrs, children) {
  return element("i", attrs, children);
}

/**
 *
 */
export function kbd(attrs, children) {
  return element("kbd", attrs, children);
}

/**
 *
 */
export function mark(attrs, children) {
  return element("mark", attrs, children);
}

/**
 *
 */
export function q(attrs, children) {
  return element("q", attrs, children);
}

/**
 *
 */
export function rp(attrs, children) {
  return element("rp", attrs, children);
}

/**
 *
 */
export function rt(attrs, children) {
  return element("rt", attrs, children);
}

/**
 *
 */
export function ruby(attrs, children) {
  return element("ruby", attrs, children);
}

/**
 *
 */
export function s(attrs, children) {
  return element("s", attrs, children);
}

/**
 *
 */
export function samp(attrs, children) {
  return element("samp", attrs, children);
}

/**
 *
 */
export function small(attrs, children) {
  return element("small", attrs, children);
}

/**
 *
 */
export function span(attrs, children) {
  return element("span", attrs, children);
}

/**
 *
 */
export function strong(attrs, children) {
  return element("strong", attrs, children);
}

/**
 *
 */
export function sub(attrs, children) {
  return element("sub", attrs, children);
}

/**
 *
 */
export function sup(attrs, children) {
  return element("sup", attrs, children);
}

/**
 *
 */
export function time(attrs, children) {
  return element("time", attrs, children);
}

/**
 *
 */
export function u(attrs, children) {
  return element("u", attrs, children);
}

/**
 *
 */
export function var$(attrs, children) {
  return element("var", attrs, children);
}

/**
 *
 */
export function wbr(attrs) {
  return element("wbr", attrs, $constants.empty_list);
}

/**
 *
 */
export function area(attrs) {
  return element("area", attrs, $constants.empty_list);
}

/**
 *
 */
export function audio(attrs, children) {
  return element("audio", attrs, children);
}

/**
 *
 */
export function img(attrs) {
  return element("img", attrs, $constants.empty_list);
}

/**
 * Used with <area> elements to define an image map (a clickable link area).
 */
export function map(attrs, children) {
  return element("map", attrs, children);
}

/**
 *
 */
export function track(attrs) {
  return element("track", attrs, $constants.empty_list);
}

/**
 *
 */
export function video(attrs, children) {
  return element("video", attrs, children);
}

/**
 *
 */
export function embed(attrs) {
  return element("embed", attrs, $constants.empty_list);
}

/**
 *
 */
export function iframe(attrs) {
  return element("iframe", attrs, $constants.empty_list);
}

/**
 *
 */
export function object(attrs) {
  return element("object", attrs, $constants.empty_list);
}

/**
 *
 */
export function picture(attrs, children) {
  return element("picture", attrs, children);
}

/**
 *
 */
export function portal(attrs) {
  return element("portal", attrs, $constants.empty_list);
}

/**
 *
 */
export function source(attrs) {
  return element("source", attrs, $constants.empty_list);
}

/**
 *
 */
export function svg(attrs, children) {
  return namespaced("http://www.w3.org/2000/svg", "svg", attrs, children);
}

/**
 *
 */
export function math(attrs, children) {
  return element("math", attrs, children);
}

/**
 *
 */
export function canvas(attrs) {
  return element("canvas", attrs, $constants.empty_list);
}

/**
 *
 */
export function noscript(attrs, children) {
  return element("noscript", attrs, children);
}

/**
 *
 */
export function script(attrs, js) {
  return $element.unsafe_raw_html("", "script", attrs, js);
}

/**
 *
 */
export function del(attrs, children) {
  return $element.element("del", attrs, children);
}

/**
 *
 */
export function ins(attrs, children) {
  return $element.element("ins", attrs, children);
}

/**
 *
 */
export function caption(attrs, children) {
  return $element.element("caption", attrs, children);
}

/**
 *
 */
export function col(attrs) {
  return $element.element("col", attrs, $constants.empty_list);
}

/**
 *
 */
export function colgroup(attrs, children) {
  return $element.element("colgroup", attrs, children);
}

/**
 *
 */
export function table(attrs, children) {
  return $element.element("table", attrs, children);
}

/**
 *
 */
export function tbody(attrs, children) {
  return $element.element("tbody", attrs, children);
}

/**
 *
 */
export function td(attrs, children) {
  return $element.element("td", attrs, children);
}

/**
 *
 */
export function tfoot(attrs, children) {
  return $element.element("tfoot", attrs, children);
}

/**
 *
 */
export function th(attrs, children) {
  return $element.element("th", attrs, children);
}

/**
 *
 */
export function thead(attrs, children) {
  return $element.element("thead", attrs, children);
}

/**
 *
 */
export function tr(attrs, children) {
  return $element.element("tr", attrs, children);
}

/**
 *
 */
export function button(attrs, children) {
  return $element.element("button", attrs, children);
}

/**
 *
 */
export function datalist(attrs, children) {
  return $element.element("datalist", attrs, children);
}

/**
 *
 */
export function fieldset(attrs, children) {
  return $element.element("fieldset", attrs, children);
}

/**
 *
 */
export function form(attrs, children) {
  return $element.element("form", attrs, children);
}

/**
 *
 */
export function input(attrs) {
  return $element.element("input", attrs, $constants.empty_list);
}

/**
 *
 */
export function label(attrs, children) {
  return $element.element("label", attrs, children);
}

/**
 *
 */
export function legend(attrs, children) {
  return $element.element("legend", attrs, children);
}

/**
 *
 */
export function meter(attrs, children) {
  return $element.element("meter", attrs, children);
}

/**
 *
 */
export function optgroup(attrs, children) {
  return $element.element("optgroup", attrs, children);
}

/**
 *
 */
export function option(attrs, label) {
  return $element.element("option", attrs, toList([$element.text(label)]));
}

/**
 *
 */
export function output(attrs, children) {
  return $element.element("output", attrs, children);
}

/**
 *
 */
export function progress(attrs, children) {
  return $element.element("progress", attrs, children);
}

/**
 *
 */
export function select(attrs, children) {
  return $element.element("select", attrs, children);
}

/**
 *
 */
export function textarea(attrs, content) {
  return $element.element(
    "textarea",
    listPrepend($attribute.property("value", $json.string(content)), attrs),
    toList([$element.text(content)]),
  );
}

/**
 *
 */
export function details(attrs, children) {
  return $element.element("details", attrs, children);
}

/**
 *
 */
export function dialog(attrs, children) {
  return $element.element("dialog", attrs, children);
}

/**
 *
 */
export function summary(attrs, children) {
  return $element.element("summary", attrs, children);
}

/**
 *
 */
export function slot(attrs, fallback) {
  return $element.element("slot", attrs, fallback);
}

/**
 *
 */
export function template(attrs, children) {
  return $element.element("template", attrs, children);
}
