// FFI for the tiramisu-renderer web component

import { Option$Some, Option$None } from "../../gleam_stdlib/gleam/option.mjs";
import { Result$Ok, Result$Error, toList } from "../../prelude.mjs";
import { setRendererBackground } from "./internal/runtime.ffi.mjs";
import { RendererConfig$RendererConfig } from "./internal/runtime.mjs";

/**
 * Get renderer configuration from the shadow root's host element.
 */
export function getRendererConfig(host) {

  const widthAttr = host.getAttribute("width");
  const heightAttr = host.getAttribute("height");

  return RendererConfig$RendererConfig(
    widthAttr ? Option$Some(parseInt(widthAttr, 10)) : Option$None(),
    heightAttr ? Option$Some(parseInt(heightAttr, 10)) : Option$None(),
    host.getAttribute("background") || "#000000",
    host.getAttribute("antialias") !== "false",
    host.getAttribute("alpha") === "true",
  );
}

/**
 * Get the user-defined scene ID from the host element's scene-id attribute.
 * Returns an empty string if not set.
 */
export function getSceneIdFromHost(host) {
  const scene_id = host.getAttribute("scene-id");
  if (scene_id) {
    return Result$Ok(scene_id);
  }
  return Result$Error();
}

/**
 * Set the scene ID on the renderer's host element and dispatch an event.
 */
export function setSceneIdOnHost(host, sceneId) {
  host.setAttribute("data-scene-id", host);

  const event = new CustomEvent("tiramisu:scene-ready", {
    bubbles: true,
    composed: true,
    detail: { sceneId }
  });
  host.dispatchEvent(event);
}

/**
 * Set the background color of a renderer.
 */
export function setBackground(rendererRef, color) {
  setRendererBackground(rendererRef.id, color);
}

/**
 * Get the host element from the shadow root.
 * Lustre's after_paint passes the shadow root — we need the host element
 * (the <tiramisu-renderer> itself) for MutationObserver and DOM parsing.
 */
export function getHostElement(shadowRoot) {
  return shadowRoot.host;
}

/**
 * Set up a MutationObserver on the host element's light DOM children.
 * Uses queueMicrotask to batch rapid DOM mutations into a single callback.
 */
export function setupMutationObserver(hostElement, callback) {
  let scheduled = false;
  const observer = new MutationObserver(() => {
    if (!scheduled) {
      scheduled = true;
      queueMicrotask(() => {
        scheduled = false;
        callback();
      });
    }
  });

  observer.observe(hostElement, {
    childList: true,
    subtree: true,
    attributes: true,
    attributeFilter: [
      "id",
      "geometry",
      "src",
      "color",
      "metalness",
      "roughness",
      "opacity",
      "wireframe",
      "transform",
      "visible",
      "physics-controlled",
      "type",
      "fov",
      "near",
      "far",
      "active",
      "intensity",
      "cast-shadow",
      "volume",
      "loop",
      "playing",
      "playback-rate",
      "ref-distance",
      "max-distance",
      "rolloff-factor",
    ],
  });

  return observer;
}

export function elementTagName(element) {
  return element.tagName
}

export function elementChildren(element) {
  return toList(element.children);
}

export function elementAttribute(element, key) {
  const val = element.getAttribute(key);
  if (val !== null) {
    return Result$Ok(val);
  }
  return Result$Error();
}
