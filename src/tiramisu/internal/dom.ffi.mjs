// Shared DOM FFI for Tiramisu web components.
//
// This module contains DOM operations that are duplicated across
// component FFI files. These are fundamentally browser-specific
// and cannot be implemented in pure Gleam.

import { Option$Some, Option$None } from "../../../gleam_stdlib/gleam/option.mjs";

/**
 * Find the parent tiramisu-renderer and get its scene ID.
 * Walks up the DOM tree looking for a tiramisu-renderer element.
 * Returns Some(sceneId) if found, None otherwise.
 */
export function findParentSceneId(shadowRoot) {
  const host = shadowRoot.host || shadowRoot;

  let element = host.parentElement;
  while (element) {
    if (element.tagName === "TIRAMISU-RENDERER") {
      const sceneId = element.getAttribute("data-scene-id");
      return sceneId ? Option$Some(sceneId) : Option$None();
    }
    element = element.parentElement;
  }

  return Option$None();
}

/**
 * Set up a listener for the scene-ready event.
 * The callback is called with the scene ID when the event fires.
 */
export function listenForSceneReady(shadowRoot, dispatch) {
  const host = shadowRoot.host || shadowRoot;

  const handler = (event) => {
    dispatch(event.detail.sceneId);
    host.removeEventListener("tiramisu:scene-ready", handler);
  };

  host.addEventListener("tiramisu:scene-ready", handler);
}

/**
 * Set an attribute on the host element.
 */
export function setHostAttribute(shadowRoot, name, value) {
  const host = shadowRoot.host || shadowRoot;
  host.setAttribute(name, value);
}

/**
 * Get an attribute from the host element.
 */
export function getHostAttribute(shadowRoot, name) {
  const host = shadowRoot.host || shadowRoot;
  const value = host.getAttribute(name);
  return value ? Option$Some(value) : Option$None();
}

/**
 * Dispatch a custom event from the host element.
 */
export function dispatchCustomEvent(shadowRoot, eventName, detail) {
  const host = shadowRoot.host || shadowRoot;
  const event = new CustomEvent(eventName, {
    bubbles: true,
    composed: true,
    detail,
  });
  host.dispatchEvent(event);
}

/**
 * Find the immediate parent tiramisu element (mesh, empty, camera, light, audio)
 * and return its ID. If no parent is found before the renderer, returns None.
 * This enables proper hierarchical transforms in the Three.js scene graph.
 */
export function findParentObjectId(shadowRoot) {
  const host = shadowRoot.host || shadowRoot;

  // Tiramisu element types that can be parents in the scene hierarchy
  const parentTypes = [
    "TIRAMISU-MESH",
    "TIRAMISU-EMPTY",
    "TIRAMISU-CAMERA",
    "TIRAMISU-LIGHT",
    "TIRAMISU-AUDIO-POSITIONAL",
  ];

  let element = host.parentElement;
  while (element) {
    // Stop at the renderer - it's the scene root
    if (element.tagName === "TIRAMISU-RENDERER") {
      return Option$None();
    }

    // Check if this is a valid parent element
    if (parentTypes.includes(element.tagName)) {
      const id = element.getAttribute("id");
      return id ? Option$Some(id) : Option$None();
    }

    element = element.parentElement;
  }

  return Option$None();
}
