// Centralized DOM FFI for Tiramisu web components.
//
// All DOM primitives live here. Higher-level DOM logic lives in dom.gleam.

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
// MUTATION OBSERVER
// ============================================================================

/**
 * Set up a MutationObserver on the host element's light DOM children.
 * Uses queueMicrotask to batch rapid DOM mutations into a single callback.
 * `observedAttrs` is a Gleam List(String) of all attribute names to watch,
 * collected from all registered extension handlers (built-in + user-defined).
 */
export function setupMutationObserver(hostElement, observedAttrs, callback) {
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
    attributeFilter: observedAttrs.toArray(),
  });
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
// ATTRIBUTE INTROSPECTION
// ============================================================================

/**
 * Get all attributes on an element as a Gleam List(#(String, String)).
 * Each entry is a Gleam tuple (two-element JS array) of [name, value].
 */
export function getAllAttributesList(element) {
  const pairs = [];
  for (const attr of element.attributes) {
    pairs.push([attr.name, attr.value]);
  }
  return pairs;
}

