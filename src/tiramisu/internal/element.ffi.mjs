import { Result$Ok, Result$Error } from "../../../prelude.mjs";
import { toList } from "../../../prelude.mjs";

export function shadowRootHost(shadowRoot) {
  return shadowRoot.host;
}

export function getAttribute(element, name) {
  const value = element.getAttribute(name);
  if (value !== null) return Result$Ok(value);
  return Result$Error(undefined);
}

export function setAttribute(element, name, value) {
  element.setAttribute(name, value);
}

export function closest(element, selector) {
  const found = element.closest(selector);
  if (found) return Result$Ok(found);
  return Result$Error(undefined);
}

export function tagName(element) {
  return element.tagName;
}

export function children(element) {
  return toList(element.children);
}

export function parentElement(element) {
  const parent = element.parentElement;
  if (parent) return Result$Ok(parent);
  return Result$Error(undefined);
}

export function setProperty(element, name, value) {
  element[name] = value;
}

export function deleteProperty(element, name) {
  delete element[name];
}

function findById(root, id) {
  if ("getElementById" in root) {
    const element = root.getElementById(id);
    if (element) return element;
  }

  if (!("querySelector" in root)) return null;
  const directMatch = root.querySelector(`#${CSS.escape(id)}`);
  if (directMatch) return directMatch;

  if (!("querySelectorAll" in root)) return null;
  for (const node of root.querySelectorAll("*")) {
    if (node.shadowRoot) {
      const shadowMatch = findById(node.shadowRoot, id);
      if (shadowMatch) return shadowMatch;
    }
  }

  return null;
}

export function getElementById(id) {
  const element = findById(document, id);
  if (element) return Result$Ok(element);
  return Result$Error(undefined);
}

export function setupMutationObserver(hostElement, observedAttrs, callback) {
  let scheduled = false;
  const observer = new MutationObserver(() => {
    if (scheduled) return;
    scheduled = true;
    queueMicrotask(() => {
      scheduled = false;
      callback();
    });
  });

  observer.observe(hostElement, {
    childList: true,
    subtree: true,
    attributes: true,
    attributeFilter: observedAttrs.toArray(),
  });
}

export function getAllAttributesList(element) {
  const pairs = [];
  for (const attr of element.attributes) {
    pairs.push([attr.name, attr.value]);
  }
  return pairs;
}

export function appendCanvasToContainer(container, canvas) {
  container.appendChild(canvas);
}
