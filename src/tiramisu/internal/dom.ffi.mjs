// Centralized DOM FFI for Tiramisu web components.
//
// All DOM primitives live here. Higher-level DOM logic lives in dom.gleam.

import { Option$Some, Option$None } from "../../../gleam_stdlib/gleam/option.mjs";
import { Result$Ok, Result$Error } from "../../../prelude.mjs";
import { toList } from "../../../prelude.mjs";

// ============================================================================
// SHADOW ROOT / HOST ACCESS
// ============================================================================

/**
 * Get the host element from a shadow root.
 * Lustre's after_paint passes the shadow root — we need the host element
 * (the <tiramisu-renderer> itself) for MutationObserver and DOM parsing.
 */
export function shadowRootHost(shadowRoot) {
  return shadowRoot.host;
}

// ============================================================================
// STANDARD DOM OPERATIONS
// ============================================================================

/**
 * Get an attribute value from an element. Returns Result(String, Nil).
 */
export function getAttribute(element, name) {
  const val = element.getAttribute(name);
  if (val !== null) return Result$Ok(val);
  return Result$Error(undefined);
}

/**
 * Set an attribute on an element.
 */
export function setAttribute(element, name, value) {
  element.setAttribute(name, value);
}

/**
 * Find the closest ancestor matching a CSS selector. Returns Result(Element, Nil).
 */
export function closest(element, selector) {
  const found = element.closest(selector);
  if (found) return Result$Ok(found);
  return Result$Error(undefined);
}

/**
 * Append a child element to a parent element.
 */
export function appendChild(parent, child) {
  parent.appendChild(child);
}

/**
 * Look up a DOM element by its ID. Returns Result(Element, Nil).
 */
export function getElementById(id) {
  const el = document.getElementById(id);
  if (el) return Result$Ok(el);
  return Result$Error(undefined);
}

// ============================================================================
// ELEMENT PROPERTY ACCESS
// ============================================================================

/**
 * Get an element's tag name (e.g. "TIRAMISU-MESH").
 */
export function tagName(element) {
  return element.tagName;
}

/**
 * Get an element's direct children as a Gleam List.
 * Converts HTMLCollection to Gleam's linked list.
 */
export function children(element) {
  return toList(element.children);
}

/**
 * Get an element's innerHTML.
 */
export function innerHTML(element) {
  return element.innerHTML;
}

/**
 * Get an element's parent element.
 * Returns Result(Element, Nil).
 */
export function parentElement(element) {
  const parent = element.parentElement;
  if (parent) return Result$Ok(parent);
  return Result$Error(undefined);
}

// ============================================================================
// INLINE STYLE
// ============================================================================

/**
 * Set an inline style property on an element.
 * e.g. setStyle(el, "position", "absolute")
 */
export function setStyle(element, property, value) {
  element.style[property] = value;
}

// ============================================================================
// CUSTOM EVENTS
// ============================================================================

/**
 * Dispatch a custom event from an element.
 * The event bubbles and is composed (crosses shadow DOM boundaries).
 */
export function dispatchCustomEvent(element, eventName, detail) {
  const event = new CustomEvent(eventName, {
    bubbles: true,
    composed: true,
    detail,
  });
  element.dispatchEvent(event);
}

// ============================================================================
// ARBITRARY JS PROPERTY ACCESS
// ============================================================================

/**
 * Set an arbitrary JavaScript property on an element.
 * Used for storing Three.js object references on DOM elements.
 */
export function setProperty(element, name, value) {
  element[name] = value;
}

/**
 * Delete an arbitrary JavaScript property from an element.
 */
export function deleteProperty(element, name) {
  delete element[name];
}

// ============================================================================
// SCENE-READY LISTENER
// ============================================================================

/**
 * Set up a one-shot listener for the scene-ready event.
 * The callback is called with the scene ID when the event fires,
 * then the listener is automatically removed.
 */
export function listenForSceneReady(host, dispatch) {
  const handler = (event) => {
    dispatch(event.detail.sceneId);
    host.removeEventListener("tiramisu:scene-ready", handler);
  };

  host.addEventListener("tiramisu:scene-ready", handler);
}

// ============================================================================
// MUTATION OBSERVER
// ============================================================================

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
      // Material properties
      "material-type",
      "emissive",
      "emissive-intensity",
      "side",
      // Texture map URLs
      "color-map",
      "normal-map",
      "ao-map",
      "roughness-map",
      "metalness-map",
      "displacement-map",
      "displacement-scale",
      "displacement-bias",
      "shininess",
      "alpha-test",
      "transparent",
      "receive-shadow",
      // Camera
      "type",
      "fov",
      "near",
      "far",
      "active",
      // Light
      "intensity",
      "cast-shadow",
      // Audio
      "volume",
      "loop",
      "playing",
      "playback-rate",
      "detune",
      "ref-distance",
      "max-distance",
      "rolloff-factor",
      // Debug
      "size",
      "divisions",
      // InstancedMesh
      "instances",
      // LOD
      "distance",
    ],
  });
}

// ============================================================================
// EVENT DETAIL CONSTRUCTORS
// ============================================================================

/**
 * Create a scene-ready event detail object with the JS-idiomatic camelCase key.
 * The event decoder in renderer.gleam expects { sceneId: "..." }.
 */
export function createSceneReadyDetail(sceneId) {
  return { sceneId };
}

/**
 * Create a mesh event detail object with the JS-idiomatic key.
 */
export function createMeshEventDetail(id) {
  return { id };
}

// ============================================================================
// CONTAINER OPERATIONS
// ============================================================================

/**
 * Append a canvas to a container (shadow root div) and set display:block.
 */
export function appendCanvasToContainer(container, canvas) {
  container.appendChild(canvas);
  canvas.style.display = "block";
}

// ============================================================================
// INTERNAL HELPERS
// ============================================================================

/**
 * Parse a string as a base-10 integer. Returns Result(Int, Nil).
 */
export function parseInt10(value) {
  const n = parseInt(value, 10);
  if (isNaN(n)) return Result$Error(undefined);
  return Result$Ok(n);
}

// ============================================================================
// DOM TREE WALKING
// ============================================================================

/**
 * Find the immediate parent tiramisu element (mesh, empty, camera, light, audio)
 * and return its ID. If no parent is found before the renderer, returns None.
 * This enables proper hierarchical transforms in the Three.js scene graph.
 *
 * This must stay in FFI because it needs to walk up the DOM tree checking
 * multiple tag names with stop-at-renderer logic that's awkward with just
 * `closest` (which can't stop at a boundary element).
 */
export function findParentObjectId(host) {
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
    // Stop at the renderer — it's the scene root
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
