// FFI for the tiramisu-renderer web component

import { Option$Some, Option$None } from "../../gleam_stdlib/gleam/option.mjs";
import { setRendererBackground } from "./internal/runtime.ffi.mjs";

/**
 * Get renderer configuration from the shadow root's host element.
 */
export function getRendererConfig(shadowRoot) {
  const host = shadowRoot.host || shadowRoot;

  const widthAttr = host.getAttribute("width");
  const heightAttr = host.getAttribute("height");

  return {
    width: widthAttr ? Option$Some(parseInt(widthAttr, 10)) : Option$None(),
    height: heightAttr ? Option$Some(parseInt(heightAttr, 10)) : Option$None(),
    background: host.getAttribute("background") || "#000000",
    antialias: host.getAttribute("antialias") !== "false",
    alpha: host.getAttribute("alpha") === "true",
    container: shadowRoot.firstElementChild || shadowRoot,
  };
}

/**
 * Get the user-defined scene ID from the host element's scene-id attribute.
 * Returns an empty string if not set.
 */
export function getSceneIdFromHost(shadowRoot) {
  const host = shadowRoot.host || shadowRoot;
  return host.getAttribute("scene-id") || "";
}

/**
 * Set the scene ID on the renderer's host element and dispatch an event.
 */
export function setSceneIdOnHost(shadowRoot, sceneId) {
  const host = shadowRoot.host || shadowRoot;
  host.setAttribute("data-scene-id", sceneId);

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
