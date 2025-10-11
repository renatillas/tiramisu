import { toList } from "../../gleam.mjs";
import * as $attribute from "../../lustre/attribute.mjs";
import * as $element from "../../lustre/element.mjs";
import { namespaced, text as inline_text } from "../../lustre/element.mjs";
import * as $constants from "../../lustre/internals/constants.mjs";

/**
 * The SVG namespace URI: `"http://www.w3.org/2000/svg"`. You might use this
 * with [`element.namespaced`](../element.html#namespaced) to create elements
 * in the SVG namespace not provided here.
 */
export const namespace = "http://www.w3.org/2000/svg";

/**
 *
 */
export function animate(attrs) {
  return namespaced(namespace, "animate", attrs, $constants.empty_list);
}

/**
 *
 */
export function animate_motion(attrs) {
  return namespaced(namespace, "animateMotion", attrs, $constants.empty_list);
}

/**
 *
 */
export function animate_transform(attrs) {
  return namespaced(namespace, "animateTransform", attrs, $constants.empty_list);
}

/**
 *
 */
export function mpath(attrs) {
  return namespaced(namespace, "mpath", attrs, $constants.empty_list);
}

/**
 *
 */
export function set(attrs) {
  return namespaced(namespace, "set", attrs, $constants.empty_list);
}

/**
 *
 */
export function circle(attrs) {
  return namespaced(namespace, "circle", attrs, $constants.empty_list);
}

/**
 *
 */
export function ellipse(attrs) {
  return namespaced(namespace, "ellipse", attrs, $constants.empty_list);
}

/**
 *
 */
export function line(attrs) {
  return namespaced(namespace, "line", attrs, $constants.empty_list);
}

/**
 *
 */
export function polygon(attrs) {
  return namespaced(namespace, "polygon", attrs, $constants.empty_list);
}

/**
 *
 */
export function polyline(attrs) {
  return namespaced(namespace, "polyline", attrs, $constants.empty_list);
}

/**
 *
 */
export function rect(attrs) {
  return namespaced(namespace, "rect", attrs, $constants.empty_list);
}

/**
 *
 */
export function a(attrs, children) {
  return namespaced(namespace, "a", attrs, children);
}

/**
 *
 */
export function defs(attrs, children) {
  return namespaced(namespace, "defs", attrs, children);
}

/**
 *
 */
export function g(attrs, children) {
  return namespaced(namespace, "g", attrs, children);
}

/**
 *
 */
export function marker(attrs, children) {
  return namespaced(namespace, "marker", attrs, children);
}

/**
 *
 */
export function mask(attrs, children) {
  return namespaced(namespace, "mask", attrs, children);
}

/**
 *
 */
export function missing_glyph(attrs, children) {
  return namespaced(namespace, "missing-glyph", attrs, children);
}

/**
 *
 */
export function pattern(attrs, children) {
  return namespaced(namespace, "pattern", attrs, children);
}

/**
 *
 */
export function svg(attrs, children) {
  return namespaced(namespace, "svg", attrs, children);
}

/**
 *
 */
export function switch$(attrs, children) {
  return namespaced(namespace, "switch", attrs, children);
}

/**
 *
 */
export function symbol(attrs, children) {
  return namespaced(namespace, "symbol", attrs, children);
}

/**
 *
 */
export function view(attrs, children) {
  return namespaced(namespace, "view", attrs, children);
}

/**
 *
 */
export function desc(attrs, children) {
  return namespaced(namespace, "desc", attrs, children);
}

/**
 *
 */
export function metadata(attrs, children) {
  return namespaced(namespace, "metadata", attrs, children);
}

/**
 *
 */
export function title(attrs, children) {
  return namespaced(namespace, "title", attrs, children);
}

/**
 *
 */
export function filter(attrs, children) {
  return namespaced(namespace, "filter", attrs, children);
}

/**
 *
 */
export function fe_blend(attrs) {
  return namespaced(namespace, "feBlend", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_color_matrix(attrs) {
  return namespaced(namespace, "feColorMatrix", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_component_transfer(attrs) {
  return namespaced(
    namespace,
    "feComponentTransfer",
    attrs,
    $constants.empty_list,
  );
}

/**
 *
 */
export function fe_composite(attrs) {
  return namespaced(namespace, "feComposite", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_convolve_matrix(attrs) {
  return namespaced(namespace, "feConvolveMatrix", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_diffuse_lighting(attrs, children) {
  return namespaced(namespace, "feDiffuseLighting", attrs, children);
}

/**
 *
 */
export function fe_displacement_map(attrs) {
  return namespaced(
    namespace,
    "feDisplacementMap",
    attrs,
    $constants.empty_list,
  );
}

/**
 *
 */
export function fe_drop_shadow(attrs) {
  return namespaced(namespace, "feDropShadow", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_flood(attrs) {
  return namespaced(namespace, "feFlood", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_func_a(attrs) {
  return namespaced(namespace, "feFuncA", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_func_b(attrs) {
  return namespaced(namespace, "feFuncB", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_func_g(attrs) {
  return namespaced(namespace, "feFuncG", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_func_r(attrs) {
  return namespaced(namespace, "feFuncR", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_gaussian_blur(attrs) {
  return namespaced(namespace, "feGaussianBlur", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_image(attrs) {
  return namespaced(namespace, "feImage", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_merge(attrs, children) {
  return namespaced(namespace, "feMerge", attrs, children);
}

/**
 *
 */
export function fe_merge_node(attrs) {
  return namespaced(namespace, "feMergeNode", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_morphology(attrs) {
  return namespaced(namespace, "feMorphology", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_offset(attrs) {
  return namespaced(namespace, "feOffset", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_specular_lighting(attrs, children) {
  return namespaced(namespace, "feSpecularLighting", attrs, children);
}

/**
 *
 */
export function fe_tile(attrs, children) {
  return namespaced(namespace, "feTile", attrs, children);
}

/**
 *
 */
export function fe_turbulence(attrs) {
  return namespaced(namespace, "feTurbulence", attrs, $constants.empty_list);
}

/**
 *
 */
export function linear_gradient(attrs, children) {
  return namespaced(namespace, "linearGradient", attrs, children);
}

/**
 *
 */
export function radial_gradient(attrs, children) {
  return namespaced(namespace, "radialGradient", attrs, children);
}

/**
 *
 */
export function stop(attrs) {
  return namespaced(namespace, "stop", attrs, $constants.empty_list);
}

/**
 *
 */
export function image(attrs) {
  return namespaced(namespace, "image", attrs, $constants.empty_list);
}

/**
 *
 */
export function path(attrs) {
  return namespaced(namespace, "path", attrs, $constants.empty_list);
}

/**
 *
 */
export function text(attrs, content) {
  return namespaced(namespace, "text", attrs, toList([$element.text(content)]));
}

/**
 *
 */
export function use_(attrs) {
  return namespaced(namespace, "use", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_distant_light(attrs) {
  return namespaced(namespace, "feDistantLight", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_point_light(attrs) {
  return namespaced(namespace, "fePointLight", attrs, $constants.empty_list);
}

/**
 *
 */
export function fe_spot_light(attrs) {
  return namespaced(namespace, "feSpotLight", attrs, $constants.empty_list);
}

/**
 *
 */
export function clip_path(attrs, children) {
  return namespaced(namespace, "clipPath", attrs, children);
}

/**
 *
 */
export function script(attrs, js) {
  return namespaced(namespace, "script", attrs, toList([inline_text(js)]));
}

/**
 *
 */
export function style(attrs, css) {
  return namespaced(namespace, "style", attrs, toList([inline_text(css)]));
}

/**
 *
 */
export function foreign_object(attrs, children) {
  return namespaced(namespace, "foreignObject", attrs, children);
}

/**
 *
 */
export function text_path(attrs, children) {
  return namespaced(namespace, "textPath", attrs, children);
}

/**
 *
 */
export function tspan(attrs, children) {
  return namespaced(namespace, "tspan", attrs, children);
}
