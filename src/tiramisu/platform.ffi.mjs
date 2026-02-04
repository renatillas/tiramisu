// FFI implementation for Tiramisu platform operations.
//
// This module provides the low-level Three.js bindings used by the platform
// to create, manipulate, and destroy scene objects.
//
// IMPORTANT: Nodes are plain JavaScript objects that can be either:
// - DOM elements (for the root container)
// - Three.js Object3D instances (for 3D objects)
// - Virtual nodes (comments, fragments)

import * as THREE from "three";
import { Result$Ok, Result$Error } from "../gleam.mjs";
import { insertMetadataChild } from "../../lustre_platform/lustre/vdom/reconciler.ffi.mjs";
import { element_kind } from "../../lustre_platform/lustre/vdom/vnode.mjs";

// DOM MANIPULATION ------------------------------------------------------------

export const appendChild = (parent, child) => {
  parent.appendChild(child);
};

export const appendCanvasToBody = (canvas) => {
  document.body.appendChild(canvas);
  // Position it absolutely to fill the viewport
  canvas.style.position = "fixed";
  canvas.style.top = "0";
  canvas.style.left = "0";
  canvas.style.width = "100%";
  canvas.style.height = "100%";
  canvas.style.zIndex = "-1"; // Behind other content
};

export const createRootFragment = () => {
  // Return a real div element instead of a fragment
  // Fragments have childNodes but not children, which breaks the reconciler
  const root = document.createElement("div");
  root.style.display = "contents"; // Makes it invisible in layout
  return root;
};

// NODE CREATION ---------------------------------------------------------------

export const createElement = (namespace, tag) => {
  // Create Three.js objects based on tag and namespace
  let object;

  switch (namespace) {
    case "debug":
      object = createDebugElement(tag);
      break;
    case "ui":
      object = createUIElement(tag);
      break;
    default:
      object = createStandardElement(tag);
  }

  if (!object) {
    throw new Error(`Failed to create element for ${namespace}:${tag}`);
  }

  // Mark the type for identification
  object.userData = object.userData || {};
  object.userData.tiramisuType = `${namespace}:${tag}`;

  return object;
};

function createStandardElement(tag) {
  let result;
  switch (tag) {
    case "scene":
      // Scene element used as root placeholder
      result = new THREE.Group();
      break;
    case "mesh":
      result = new THREE.Mesh();
      break;
    case "camera":
      result = new THREE.PerspectiveCamera(75, 1, 0.1, 1000);
      break;
    case "light":
      result = new THREE.AmbientLight(0xffffff, 1);
      break;
    case "empty":
      result = new THREE.Group();
      break;
    case "sprite":
      result = new THREE.Sprite();
      break;
    case "model":
      result = new THREE.Group();
      break;
    case "audio":
      result = new THREE.Object3D();
      break;
    case "lod":
      result = new THREE.LOD();
      break;
    case "instanced-mesh":
      result = new THREE.Group(); // Placeholder
      break;
    case "instanced-model":
      result = new THREE.Group();
      break;
    case "animated-sprite":
      result = new THREE.Sprite();
      break;
    // Handle HTML elements
    case "div":
    case "span":
    case "p":
      result = document.createElement(tag);
      break;
    default:
      console.warn(`Unknown tag: ${tag}, creating Group`);
      result = new THREE.Group(); // Default to empty group
  }

  if (!result) {
    throw new Error(`Failed to create element for tag: ${tag}`);
  }

  return result;
}

function createUIElement(tag) {
  switch (tag) {
    case "css2d":
    case "css3d":
    case "canvas":
      return new THREE.Object3D();
    default:
      return new THREE.Object3D();
  }
}

function createDebugElement(tag) {
  switch (tag) {
    case "box":
    case "sphere":
    case "line":
    case "point":
      return new THREE.Group();
    case "axes":
      return new THREE.AxesHelper(1);
    case "grid":
      return new THREE.GridHelper(10, 10);
    default:
      return new THREE.Group();
  }
}

export const createTextNode = (content) => {
  // Return an actual DOM text node since the root is DOM
  return document.createTextNode(content);
};

export const createFragment = () => {
  // Return a DOM document fragment
  return document.createDocumentFragment();
};

export const createComment = (text) => {
  // Return a DOM comment node
  return document.createComment(text);
};

// SCENE GRAPH MANIPULATION ----------------------------------------------------

// Helper to check if a node is a DOM element
function isDOMElement(node) {
  return node && node.nodeType !== undefined;
}

// Helper to unwrap Result type from Gleam
function unwrapResult(result) {
  if (result && result.isOk) return result[0];
  return null;
}

export const insertBefore = (parent, node, reference) => {
  if (!parent || !node) {
    return;
  }

  // Handle DOM elements (root container)
  // When parent is DOM, add Three.js objects to the global scene instead
  if (isDOMElement(parent)) {
    if (node.isObject3D && globalScene) {
      globalScene.add(node);
    }
    // Don't try to add Three.js objects to DOM
    return;
  }

  // Handle Three.js Object3D (for nested scene graph)
  if (parent.isObject3D) {
    const refNode = unwrapResult(reference);
    if (refNode && refNode.parent === parent) {
      const refIndex = parent.children.indexOf(refNode);
      parent.children.splice(refIndex, 0, node);
      node.parent = parent;
    } else {
      parent.add(node);
    }
  }
};

export const moveBefore = (parent, node, reference) => {
  // For Three.js, move is same as insert (it automatically removes from old parent)
  insertBefore(parent, node, reference);
};

export const removeChild = (parent, child) => {
  if (isDOMElement(parent)) {
    // When parent is DOM, remove Three.js objects from the global scene
    if (child.isObject3D && globalScene) {
      globalScene.remove(child);
    }
    // Don't try to remove Three.js objects from DOM
  } else if (parent.isObject3D) {
    parent.remove(child);
  }
};

export const nextSibling = (node) => {
  if (!node) {
    return Result$Error(undefined);
  }

  // For DOM elements
  if (isDOMElement(node)) {
    const sibling = node.nextSibling;
    return sibling ? Result$Ok(sibling) : Result$Error(undefined);
  }

  // For Three.js objects
  if (node.parent) {
    const index = node.parent.children.indexOf(node);
    if (index !== -1 && index < node.parent.children.length - 1) {
      return Result$Ok(node.parent.children[index + 1]);
    }
  }

  return Result$Error(undefined);
};

// ATTRIBUTES ------------------------------------------------------------------

export const getAttribute = (node, name) => {
  if (!node) {
    return Result$Error(undefined);
  }

  // For DOM elements
  if (isDOMElement(node)) {
    const value = node.getAttribute(name);
    return value ? Result$Ok(value) : Result$Error(undefined);
  }

  // For Three.js objects, check userData
  const value = node.userData && node.userData[name];
  return value !== undefined ? Result$Ok(String(value)) : Result$Error(undefined);
};

export const setAttribute = (node, name, value) => {
  if (!node) {
    return;
  }

  // For DOM elements
  if (isDOMElement(node)) {
    node.setAttribute(name, value);
    return;
  }

  // For Three.js objects, store in userData
  node.userData = node.userData || {};
  node.userData[name] = value;

  // Parse and apply specific attributes
  applyAttribute(node, name, value);
};

function applyAttribute(object, name, value) {
  // Parse hierarchical attribute names and apply to Three.js object
  if (name.startsWith("data-position")) {
    const [x, y, z] = value.split(",").map(parseFloat);
    object.position.set(x, y, z);
  } else if (name.startsWith("data-rotation")) {
    const [x, y, z] = value.split(",").map(parseFloat);
    object.rotation.set(x, y, z);
  } else if (name.startsWith("data-scale")) {
    const [x, y, z] = value.split(",").map(parseFloat);
    object.scale.set(x, y, z);
  } else if (name.startsWith("data-color")) {
    // Apply color to material
    if (object.material) {
      object.material.color = new THREE.Color(value);
    }
  } else if (name.startsWith("data-geometry")) {
    // Parse geometry type and create
    const parts = value.split(":");
    const type = parts[0];
    const params = parts[1] ? parts[1].split(",").map(parseFloat) : [];

    switch (type) {
      case "box":
        object.geometry = new THREE.BoxGeometry(params[0] || 1, params[1] || 1, params[2] || 1);
        break;
      case "sphere":
        object.geometry = new THREE.SphereGeometry(params[0] || 1, params[1] || 32, params[2] || 16);
        break;
      case "plane":
        object.geometry = new THREE.PlaneGeometry(params[0] || 1, params[1] || 1);
        break;
      case "cylinder":
        object.geometry = new THREE.CylinderGeometry(
          params[0] || 1,
          params[1] || 1,
          params[2] || 1,
          params[3] || 32
        );
        break;
    }

    // Create default material if mesh doesn't have one
    if (object.isMesh && !object.material) {
      object.material = new THREE.MeshStandardMaterial({ color: 0xffffff });
    }
  }
}

export const removeAttribute = (node, name) => {
  if (isDOMElement(node)) {
    node.removeAttribute(name);
  } else if (node.userData) {
    delete node.userData[name];
  }
};

export const setProperty = (node, name, value) => {
  node[name] = value;
};

// CONTENT ---------------------------------------------------------------------

export const setText = (node, content) => {
  if (isDOMElement(node)) {
    node.textContent = content;
  } else {
    node.userData = node.userData || {};
    node.userData.textContent = content;
  }
};

export const setRawContent = (node, content) => {
  if (isDOMElement(node)) {
    node.innerHTML = content;
  } else {
    node.userData = node.userData || {};
    node.userData.rawContent = content;
  }
};

// EVENTS ----------------------------------------------------------------------

export const addEventListener = (node, eventName, handler, passive) => {
  if (isDOMElement(node)) {
    node.addEventListener(eventName, handler, { passive });
  } else {
    // Store event listeners on Three.js objects for raycasting
    node.userData = node.userData || {};
    node.userData.eventListeners = node.userData.eventListeners || {};
    node.userData.eventListeners[eventName] = node.userData.eventListeners[eventName] || [];
    node.userData.eventListeners[eventName].push({ handler, passive });
  }
};

export const removeEventListener = (node, eventName, handler) => {
  if (isDOMElement(node)) {
    node.removeEventListener(eventName, handler);
  } else if (node.userData && node.userData.eventListeners) {
    const listeners = node.userData.eventListeners[eventName];
    if (listeners) {
      node.userData.eventListeners[eventName] = listeners.filter((l) => l.handler !== handler);
    }
  }
};

// SCHEDULING ------------------------------------------------------------------

export const scheduleRender = (callback) => {
  const id = window.requestAnimationFrame(callback);
  return () => window.cancelAnimationFrame(id);
};

// DEBUG -----------------------------------------------------------------------

export const debugLog = (message, value) => {
  console.log(message, value, "has children:", !!value?.children, "children:", value?.children);
};

// SCENE MANAGEMENT ------------------------------------------------------------

let globalScene = null;

export const setGlobalScene = (scene) => {
  globalScene = scene;
};

export const clearContainer = (container) => {
  // Remove all child nodes
  while (container.firstChild) {
    container.removeChild(container.firstChild);
  }
  // Remove all attributes except id
  const id = container.id;
  while (container.attributes.length > 0) {
    container.removeAttribute(container.attributes[0].name);
  }
  if (id) {
    container.id = id;
  }
};

export const initReconcilerMetadata = (root) => {
  // Initialize the reconciler metadata on the root node
  // This is critical for the reconciler to function properly
  insertMetadataChild(element_kind, null, root, 0, null);
};
