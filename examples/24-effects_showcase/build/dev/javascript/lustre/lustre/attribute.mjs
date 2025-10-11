import * as $json from "../../gleam_json/gleam/json.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Empty as $Empty } from "../gleam.mjs";
import * as $vattr from "../lustre/vdom/vattr.mjs";

/**
 * Create an HTML attribute. This is like saying `element.setAttribute("class", "wibble")`
 * in JavaScript. Attributes will be rendered when calling [`element.to_string`](./element.html#to_string).
 *
 * > **Note**: there is a subtle difference between attributes and properties. You
 * > can read more about the implications of this
 * > [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
 */
export function attribute(name, value) {
  return $vattr.attribute(name, value);
}

/**
 * Create a DOM property. This is like saying `element.className = "wibble"` in
 * JavaScript. Properties will be **not** be rendered when calling
 * [`element.to_string`](./element.html#to_string).
 *
 * > **Note**: there is a subtle difference between attributes and properties. You
 * > can read more about the implications of this
 * > [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).
 */
export function property(name, value) {
  return $vattr.property(name, value);
}

function boolean_attribute(name, value) {
  if (value) {
    return attribute(name, "");
  } else {
    return property(name, $json.bool(false));
  }
}

/**
 * Defines a shortcut key to activate or focus the element. Multiple options
 * may be provided as a set of space-separated characters that are exactly one
 * code point each.
 *
 * The way to activate the access key depends on the browser and its platform:
 *
 * |         | Windows           | Linux               | Mac OS              |
 * |---------|-------------------|---------------------|---------------------|
 * | Firefox | Alt + Shift + key | Alt + Shift + key   | Ctrl + Option + key |
 * | Chrome  | Alt + key         | Ctrl + Option + key | Ctrl + Option + key |
 * | Safari  |                   |                     | Ctrl + Option + key |
 */
export function accesskey(key) {
  return attribute("accesskey", key);
}

/**
 * Controls whether text input is automatically capitalised. The following values
 * are accepted:
 *
 * | Value        | Mode       |
 * |--------------|------------|
 * | ""           | default    |
 * | "none"       | none       |
 * | "off"        |            |
 * | "sentences"  | sentences  |
 * | "on"         |            |
 * | "words"      | words      |
 * | "characters" | characters |
 *
 * The autocapitalisation processing model is based on the following five modes:
 *
 * - **default**: The user agent and input method should make their own determination
 *   of whether or not to enable autocapitalization.
 *
 * - **none**: No autocapitalisation should be applied (all letters should default
 *   to lowercase).
 *
 * - **sentences**: The first letter of each sentence should default to a capital
 *   letter; all other letters should default to lowercase.
 *
 * - **words**: The first letter of each word should default to a capital letter;
 *   all other letters should default to lowercase.
 *
 * - **characters**: All letters should default to uppercase.
 */
export function autocapitalize(value) {
  return attribute("autocapitalize", value);
}

/**
 * Controls whether the user agent may automatically correct mispelled words
 * while typing. Whether or not spelling is corrected is left up to the user
 * agent and may also depend on the user's settings.
 *
 * When disabled the user agent is **never** allowed to correct spelling.
 */
export function autocorrect(enabled) {
  return boolean_attribute("autocorrect", enabled);
}

/**
 * For server-rendered HTML, this attribute controls whether an element should
 * be focused when the page first loads.
 *
 * > **Note**: Lustre's runtime augments that native behaviour of this attribute.
 * > Whenever it is toggled true, the element will be automatically focused even
 * > if it already exists in the DOM.
 */
export function autofocus(should_autofocus) {
  return boolean_attribute("autofocus", should_autofocus);
}

/**
 * A class is a non-unique identifier for an element primarily used for styling
 * purposes. You can provide multiple classes as a space-separated list and any
 * style rules that apply to any of the classes will be applied to the element.
 *
 * To conditionally toggle classes on and off, you can use the [`classes`](#classes)
 * function instead.
 *
 * > **Note**: unlike most attributes, multiple `class` attributes are merged
 * > with any existing other classes on an element. Classes added _later_ in the
 * > list will override classes added earlier.
 */
export function class$(name) {
  return attribute("class", name);
}

/**
 * Create an empty attribute. This is not added to the DOM and not rendered when
 * calling [`element.to_string`](./element.html#to_string), but it is useful for
 * _conditionally_ adding attributes to an element.
 */
export function none() {
  return class$("");
}

function do_classes(loop$names, loop$class) {
  while (true) {
    let names = loop$names;
    let class$ = loop$class;
    if (names instanceof $Empty) {
      return class$;
    } else {
      let $ = names.head[1];
      if ($) {
        let rest = names.tail;
        let name$1 = names.head[0];
        return ((class$ + name$1) + " ") + do_classes(rest, class$);
      } else {
        let rest = names.tail;
        loop$names = rest;
        loop$class = class$;
      }
    }
  }
}

/**
 * A class is a non-unique identifier for an element primarily used for styling
 * purposes. You can provide multiple classes as a space-separated list and any
 * style rules that apply to any of the classes will be applied to the element.
 * This function allows you to conditionally toggle classes on and off.
 *
 * > **Note**: unlike most attributes, multiple `class` attributes are merged
 * > with any existing other classes on an element. Classes added _later_ in the
 * > list will override classes added earlier.
 */
export function classes(names) {
  return class$(do_classes(names, ""));
}

/**
 * Indicates whether the element's content is editable by the user, allowing them
 * to modify the HTML content directly. The following values are accepted:
 *
 * | Value        | Description                                           |
 * |--------------|-------------------------------------------------------|
 * | "true"       | The element is editable.                              |
 * | ""           |                                                       |
 * | "false"      | The element is not editable.                          |
 * | "plain-text" | The element is editable without rich text formatting. |
 *
 * > **Note**: setting the value to an empty string does *not* disable this
 * > attribute, and is instead equivalent to setting it to `"true"`!
 */
export function contenteditable(is_editable) {
  return attribute("contenteditable", is_editable);
}

/**
 * Add a `data-*` attribute to an HTML element. The key will be prefixed by
 * `"data-"`, and accessible from JavaScript or in Gleam decoders under the
 * path `element.dataset.key` where `key` is the key you provide to this
 * function.
 */
export function data(key, value) {
  return attribute("data-" + key, value);
}

/**
 * Specifies the text direction of the element's content. The following values
 * are accepted:
 *
 * | Value  | Description                                                          |
 * |--------|----------------------------------------------------------------------|
 * | "ltr"  | The element's content is left-to-right.                              |
 * | "rtl"  | The element's content is right-to-left.                              |
 * | "auto" | The element's content direction is determined by the content itself. |
 *
 * > **Note**: the `"auto"` value should only be used as a last resort in cases
 * > where the content's direction is truly unknown. The heuristic used by
 * > browsers is naive and only considers the first character available that
 * > indicates the direction.
 */
export function dir(direction) {
  return attribute("dir", direction);
}

/**
 * Indicates whether the element can be dragged as part of the HTML drag-and-drop
 * API.
 */
export function draggable(is_draggable) {
  return attribute(
    "draggable",
    (() => {
      if (is_draggable) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * Specifies what action label (or potentially icon) to present for the "enter"
 * key on virtual keyboards such as mobile devices. The following values are
 * accepted:
 *
 * | Value      | Example        |
 * |------------|----------------|
 * | "enter"    | "return", "↵"  |
 * | "done"     | "done", "✅"   |
 * | "go"       | "go"           |
 * | "next"     | "next"         |
 * | "previous" | "return"       |
 * | "search"   | "search", "🔍" |
 * | "send"     | "send"         |
 *
 * The examples listed are demonstrative and may not be the actual labels used
 * by user agents. When unspecified or invalid, the user agent may use contextual
 * information such as the type of an input to determine the label.
 */
export function enterkeyhint(value) {
  return attribute("enterkeyhint", value);
}

/**
 * Indicates whether the element is relevant to the page's current state. A
 * hidden element is not visible to the user and is inaccessible to assistive
 * technologies such as screen readers. This makes it unsuitable for simple
 * presentation purposes, but it can be useful for example to render something
 * that may be made visible later.
 */
export function hidden(is_hidden) {
  return boolean_attribute("hidden", is_hidden);
}

/**
 * The `"id"` attribute is used to uniquely identify a single element within a
 * document. It can be used to reference the element in CSS with the selector
 * `#id`, in JavaScript with `document.getElementById("id")`, or by anchors on
 * the same page with the URL `"#id"`.
 */
export function id(value) {
  return attribute("id", value);
}

/**
 * Marks the element as inert, meaning it is not currently interactive and does
 * not receive user input. For sighted users, it's common to style inert elements
 * in a way that makes them visually distinct from active elements, such as by
 * greying them out: this can help avoid confusion for users who may not otherwise
 * know the content they are looking at is inactive.
 */
export function inert(is_inert) {
  return boolean_attribute("inert", is_inert);
}

/**
 * Hints to the user agent about what type of virtual keyboard to display when
 * the user interacts with the element. The following values are accepted:
 *
 * | Value        | Description                                                   |
 * |--------------|---------------------------------------------------------------|
 * | "none"       | No virtual keyboard should be displayed.                      |
 * | "text"       | A standard text input keyboard.                               |
 * | "decimal"    | A numeric keyboard with locale-appropriate separator.         |
 * | "numeric"    | A numeric keyboard.                                           |
 * | "tel"        | A telephone keypad including "#" and "*".                     |
 * | "email"      | A keyboard for entering email addresses including "@" and "." |
 * | "url"        | A keyboard for entering URLs including "/" and ".".           |
 * | "search"     | A keyboard for entering search queries should be shown.       |
 *
 * The `"none"` value should only be used in cases where you are rendering a
 * custom input method, otherwise the user will not be able to enter any text!
 */
export function inputmode(value) {
  return attribute("inputmode", value);
}

/**
 * Specifies the [customised built-in element](https://html.spec.whatwg.org/#customized-built-in-element)
 * to be used in place of the native element this attribute is applied to.
 */
export function is(value) {
  return attribute("is", value);
}

/**
 * Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
 * specify the global unique identifier of an item, for example books that are
 * identifiable by their ISBN.
 */
export function itemid(id) {
  return attribute("itemid", id);
}

/**
 * Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
 * specify that the content of the element is to be treated as a value of the
 * given property name.
 */
export function itemprop(name) {
  return attribute("itemprop", name);
}

/**
 * Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
 * indicate that the element and its descendants form a single item of key-value
 * data.
 */
export function itemscope(has_scope) {
  return boolean_attribute("itemscope", has_scope);
}

/**
 * Used as part of the [Microdata](https://schema.org/docs/gs.html) format to
 * specify the type of item being described. This is a URL that points to
 * a schema containing the vocabulary used for an item's key-value pairs, such
 * as a schema.org type.
 */
export function itemtype(url) {
  return attribute("itemtype", url);
}

/**
 * Specifies the language of the element's content and the language of any of
 * this element's attributes that contain text. The `"lang"` attribute applies
 * to the element itself and all of its descendants, unless overridden by
 * another `"lang"` attribute on a descendant element.
 *
 * The value must be a valid [BCP 47 language tag](https://tools.ietf.org/html/bcp47).
 */
export function lang(language) {
  return attribute("lang", language);
}

/**
 * A cryptographic nonce used by CSP (Content Security Policy) to allow or
 * deny the fetch of a given resource.
 */
export function nonce(value) {
  return attribute("nonce", value);
}

/**
 * Specifies that the element should be treated as a popover, rendering it in
 * the top-layer above all other content when the popover is active. The following
 * values are accepted:
 *
 * | Value        | Description                                    |
 * |--------------|------------------------------------------------|
 * | "auto"       | Closes other popovers when opened.             |
 * | ""           |                                                |
 * | "manual"     | Does not close other popovers when opened.     |
 * | "hint"       | Closes only other "hint" popovers when opened. |
 *
 * All modes except `"manual"` support "light dismiss" letting the user close
 * the popover by clicking outside of it, as well as respond to close requests
 * letting the user dismiss a popover by pressing the "escape" key or by using
 * the dismiss gesture on any assistive technology.
 *
 * Popovers can be triggered either programmatically through the `showPopover()`
 * method, or by assigning an [`id`](#id) to the element and including the
 * [`popovertarget`](#popovertarget) attribute on the element that should trigger
 * the popover.
 */
export function popover(value) {
  return attribute("popover", value);
}

/**
 * Indicates whether the element's content should be checked for spelling errors.
 * This typically only applies to inputs and textareas, or elements that are
 * [`contenteditable`](#contenteditable).
 */
export function spellcheck(should_check) {
  return attribute(
    "spellcheck",
    (() => {
      if (should_check) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * Provide a single property name and value to be used as inline styles for the
 * element. If either the property name or value is empty, this attribute will
 * be ignored.
 *
 * > **Note**: unlike most attributes, multiple `style` attributes are merged
 * > with any existing other styles on an element. Styles added _later_ in the
 * > list will override styles added earlier.
 */
export function style(property, value) {
  if (property === "") {
    return class$("");
  } else if (value === "") {
    return class$("");
  } else {
    return attribute("style", ((property + ":") + value) + ";");
  }
}

function do_styles(loop$properties, loop$styles) {
  while (true) {
    let properties = loop$properties;
    let styles = loop$styles;
    if (properties instanceof $Empty) {
      return styles;
    } else {
      let $ = properties.head[0];
      if ($ === "") {
        let rest = properties.tail;
        loop$properties = rest;
        loop$styles = styles;
      } else {
        let $1 = properties.head[1];
        if ($1 === "") {
          let rest = properties.tail;
          loop$properties = rest;
          loop$styles = styles;
        } else {
          let rest = properties.tail;
          let name$1 = $;
          let value$1 = $1;
          loop$properties = rest;
          loop$styles = (((styles + name$1) + ":") + value$1) + ";";
        }
      }
    }
  }
}

/**
 * Provide a list of property-value pairs to be used as inline styles for the
 * element. Empty properties or values are omitted from the final style string.
 *
 * > **Note**: unlike most attributes, multiple `styles` attributes are merged
 * > with any existing other styles on an element. Styles added _later_ in the
 * > list will override styles added earlier.
 */
export function styles(properties) {
  return attribute("style", do_styles(properties, ""));
}

/**
 * Specifies the tabbing order of the element. If an element is not typically
 * focusable, such as a `<div>`, it will be made focusable when this attribute
 * is set.
 *
 * Any integer value is accepted, but the following values are recommended:
 *
 * - `-1`: indicates the element may receive focus, but should not be sequentially
 *   focusable. The user agent may choose to ignore this preference if, for
 *   example, the user agent is a screen reader.
 *
 * - `0`: indicates the element may receive focus and should be placed in the
 *   sequential focus order in the order it appears in the DOM.
 *
 * - any positive integer: indicates the element should be placed in the sequential
 *   focus order relative to other elements with a positive tabindex.
 *
 * Values other than `0` and `-1` are generally not recommended as managing
 * the relative order of focusable elements can be difficult and error-prone.
 */
export function tabindex(index) {
  return attribute("tabindex", $int.to_string(index));
}

/**
 * Annotate an element with additional information that may be suitable as a
 * tooltip, such as a description of a link or image.
 *
 * It is **not** recommended to use the `title` attribute as a way of providing
 * accessibility information to assistive technologies. User agents often do not
 * expose the `title` attribute to keyboard-only users or touch devices, for
 * example.
 */
export function title(text) {
  return attribute("title", text);
}

/**
 * Controls whether an element's content may be translated by the user agent
 * when the page is localised. This includes both the element's text content
 * and some of its attributes:
 *
 * | Attribute   | Element(s)                                 |
 * |-------------|--------------------------------------------|
 * | abbr        | th                                         |
 * | alt         | area, img, input                           |
 * | content     | meta                                       |
 * | download    | a, area                                    |
 * | label       | optgroup, option, track                    |
 * | lang        | *                                          |
 * | placeholder | input, textarea                            |
 * | srcdoc      | iframe                                     |
 * | title       | *                                          |
 * | style       | *                                          |
 * | value       | input (with type="button" or type="reset") |
 */
export function translate(should_translate) {
  return attribute(
    "translate",
    (() => {
      if (should_translate) {
        return "yes";
      } else {
        return "no";
      }
    })(),
  );
}

/**
 * Indicates if writing suggestions should be enabled for this element.
 */
export function writingsuggestions(enabled) {
  return attribute(
    "writingsuggestions",
    (() => {
      if (enabled) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * Specifies the URL of a linked resource. This attribute can be used on various
 * elements to create hyperlinks or to load resources.
 */
export function href(url) {
  return attribute("href", url);
}

/**
 * Specifies where to display the linked resource or where to open the link.
 * The following values are accepted:
 *
 * | Value     | Description                                             |
 * |-----------|---------------------------------------------------------|
 * | "_self"   | Open in the same frame/window (default)                 |
 * | "_blank"  | Open in a new window or tab                             |
 * | "_parent" | Open in the parent frame                                |
 * | "_top"    | Open in the full body of the window                     |
 * | framename | Open in a named frame                                   |
 *
 * > **Note**: consider against using `"_blank"` for links to external sites as it
 * > removes user control over their browsing experience.
 */
export function target(value) {
  return attribute("target", value);
}

/**
 * Indicates that the linked resource should be downloaded rather than displayed.
 * When provided with a value, it suggests a filename for the downloaded file.
 */
export function download(filename) {
  return attribute("download", filename);
}

/**
 * Provides a space-separated list of URLs that will be notified if the user
 * follows the hyperlink. These URLs will receive POST requests with bodies
 * of type `ping/1.0`.
 */
export function ping(urls) {
  return attribute("ping", $string.join(urls, " "));
}

/**
 * Specifies the relationship between the current document and the linked resource.
 * Multiple relationship values can be provided as a space-separated list.
 */
export function rel(value) {
  return attribute("rel", value);
}

/**
 * Specifies the language of the linked resource. The value must be a valid
 * [BCP 47 language tag](https://tools.ietf.org/html/bcp47).
 */
export function hreflang(language) {
  return attribute("hreflang", language);
}

/**
 * Specifies the referrer policy for fetches initiated by the element. The
 * following values are accepted:
 *
 * | Value                              | Description                                           |
 * |-----------------------------------|--------------------------------------------------------|
 * | "no-referrer"                     | No Referer header is sent                              |
 * | "no-referrer-when-downgrade"      | Only send Referer for same-origin or more secure       |
 * | "origin"                          | Send only the origin part of the URL                   |
 * | "origin-when-cross-origin"        | Full URL for same-origin, origin only for cross-origin |
 * | "same-origin"                     | Only send Referer for same-origin requests             |
 * | "strict-origin"                   | Like origin, but only to equally secure destinations   |
 * | "strict-origin-when-cross-origin" | Default policy with varying levels of restriction      |
 * | "unsafe-url"                      | Always send the full URL                               |
 */
export function referrerpolicy(value) {
  return attribute("referrerpolicy", value);
}

/**
 * Specifies text that should be displayed when the image cannot be rendered.
 * This attribute is essential for accessibility, providing context about the
 * image to users who cannot see it, including those using screen readers.
 */
export function alt(text) {
  return attribute("alt", text);
}

/**
 * Specifies the URL of an image or resource to be used.
 */
export function src(url) {
  return attribute("src", url);
}

/**
 * Specifies a set of image sources for different display scenarios. This allows
 * browsers to choose the most appropriate image based on factors like screen
 * resolution and viewport size.
 */
export function srcset(sources) {
  return attribute("srcset", sources);
}

/**
 * Used with `srcset` to define the size of images in different layout scenarios.
 * Helps the browser select the most appropriate image source.
 */
export function sizes(value) {
  return attribute("sizes", value);
}

/**
 * Configures the CORS (Cross-Origin Resource Sharing) settings for the element.
 * Valid values are "anonymous" and "use-credentials".
 */
export function crossorigin(value) {
  return attribute("crossorigin", value);
}

/**
 * Specifies the name of an image map to be used with the image.
 */
export function usemap(value) {
  return attribute("usemap", value);
}

/**
 * Indicates that the image is a server-side image map. When a user clicks on the
 * image, the coordinates of the click are sent to the server.
 */
export function ismap(is_map) {
  return boolean_attribute("ismap", is_map);
}

/**
 * Specifies the width of the element in pixels.
 */
export function width(value) {
  return attribute("width", $int.to_string(value));
}

/**
 * Specifies the height of the element in pixels.
 */
export function height(value) {
  return attribute("height", $int.to_string(value));
}

/**
 * Provides a hint about how the image should be decoded. Valid values are
 * "sync", "async", and "auto".
 */
export function decoding(value) {
  return attribute("decoding", value);
}

/**
 * Indicates how the browser should load the image. Valid values are "eager"
 * (load immediately) and "lazy" (defer loading until needed).
 */
export function loading(value) {
  return attribute("loading", value);
}

/**
 * Sets the priority for fetches initiated by the element. Valid values are
 * "high", "low", and "auto".
 */
export function fetchpriority(value) {
  return attribute("fetchpriority", value);
}

/**
 * Specifies the character encodings to be used for form submission. This allows
 * servers to know how to interpret the form data. Multiple encodings can be
 * specified as a space-separated list.
 */
export function accept_charset(charsets) {
  return attribute("accept-charset", charsets);
}

/**
 * Specifies the URL to which the form's data should be sent when submitted.
 * This can be overridden by formaction attributes on submit buttons.
 */
export function action(url) {
  return attribute("action", url);
}

/**
 * Specifies how form data should be encoded before sending it to the server.
 * Valid values include:
 *
 * | Value                             | Description                           |
 * |-----------------------------------|---------------------------------------|
 * | "application/x-www-form-urlencoded" | Default encoding (spaces as +, etc.)  |
 * | "multipart/form-data"               | Required for file uploads             |
 * | "text/plain"                        | Simple encoding with minimal escaping  |
 */
export function enctype(encoding_type) {
  return attribute("enctype", encoding_type);
}

/**
 * Specifies the HTTP method to use when submitting the form. Common values are:
 *
 * | Value    | Description                                              |
 * |----------|----------------------------------------------------------|
 * | "get"    | Appends form data to URL (default)                       |
 * | "post"   | Sends form data in the body of the HTTP request          |
 * | "dialog" | Closes a dialog if the form is inside one                |
 */
export function method(http_method) {
  return attribute("method", http_method);
}

/**
 * When present, indicates that the form should not be validated when submitted.
 * This allows submission of forms with invalid or incomplete data.
 */
export function novalidate(disable_validation) {
  return boolean_attribute("novalidate", disable_validation);
}

/**
 * A hint for the user agent about what file types are expected to be submitted.
 * The following values are accepted:
 *
 * | Value     | Description                                          |
 * |-----------|------------------------------------------------------|
 * | "audio/*" | Any audio file type.                                 |
 * | "video/*" | Any video file type.                                 |
 * | "image/*" | Any image file type.                                 |
 * | mime/type | A complete MIME type, without additional parameters. |
 * | .ext      | Indicates any file with the given extension.         |
 *
 * The following input types support the `"accept"` attribute:
 *
 * - `"file"`
 *
 * > **Note**: the `"accept"` attribute is a *hint* to the user agent and does
 * > not guarantee that the user will only be able to select files of the specified
 * > type.
 */
export function accept(values) {
  return attribute("accept", $string.join(values, ","));
}

/**
 * Allow a colour's alpha component to be manipulated, allowing the user to
 * select a colour with transparency.
 *
 * The following input types support the `"alpha"` attribute:
 *
 * - `"color"`
 */
export function alpha(allowed) {
  return boolean_attribute("alpha", allowed);
}

/**
 * A hint for the user agent to automatically fill the value of the input with
 * an appropriate value. The format for the `"autocomplete"` attribute is a
 * space-separated ordered list of optional tokens:
 *
 *     "section-* (shipping | billing) [...fields] webauthn"
 *
 * - `section-*`: used to disambiguate between two fields with otherwise identical
 *   autocomplete values. The `*` is replaced with a string that identifies the
 *   section of the form.
 *
 * - `shipping | billing`: indicates the field is related to shipping or billing
 *   address or contact information.
 *
 * - `[...fields]`: a space-separated list of field names that are relevant to
 *   the input, for example `"email"`, `"name family-name"`, or `"home tel"`.
 *
 * - `webauthn`: indicates the field can be automatically filled with a WebAuthn
 *   passkey.
 *
 * In addition, the value may instead be `"off"` to disable autocomplete for the
 * input, or `"on"` to let the user agent decide based on context what values
 * are appropriate.
 *
 * The following input types support the `"autocomplete"` attribute:
 *
 * - `"color"`
 * - `"date"`
 * - `"datetime-local"`
 * - `"email"`
 * - `"hidden"`
 * - `"month"`
 * - `"number"`
 * - `"password"`
 * - `"range"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"time"`
 * - `"url"`
 * - `"week"`
 */
export function autocomplete(value) {
  return attribute("autocomplete", value);
}

/**
 * Whether the control is checked or not. When participating in a form, the
 * value of the input is included in the form submission if it is checked. For
 * checkboxes that do not have a value, the value of the input is `"on"` when
 * checked.
 *
 * The following input types support the `"checked"` attribute:
 *
 * - `"checkbox"`
 * - `"radio"`
 */
export function checked(is_checked) {
  return boolean_attribute("checked", is_checked);
}

/**
 * The color space of the serialised CSS color. It also hints to user agents
 * about what kind of interface to present to the user for selecting a color.
 * The following values are accepted:
 *
 * - `"limited-srgb"`: The CSS color is converted to the 'srgb' color space and
 *   limited to 8-bits per component, e.g., `"#123456"` or
 *   `"color(srgb 0 1 0 / 0.5)"`.
 *
 * - `"display-p3"`: The CSS color is converted to the 'display-p3' color space,
 *   e.g., `"color(display-p3 1.84 -0.19 0.72 / 0.6)"`.
 *
 * The following input types support the `"colorspace"` attribute:
 *
 * - `"color"`
 */
export function colorspace(value) {
  return attribute("colorspace", value);
}

/**
 * The name of the field included in a form that indicates the direcionality of
 * the user's input.
 *
 * The following input types support the `"dirname"` attribute:
 *
 * - `"email"`
 * - `"hidden"`
 * - `"password"`
 * - `"search"`
 * - `"submit"
 * - `"tel"`
 * - `"text"`
 * - `"url"`
 */
export function dirname(direction) {
  return attribute("dirname", direction);
}

/**
 * Controls whether or not the input is disabled. Disabled inputs are not
 * validated during form submission and are not interactive.
 */
export function disabled(is_disabled) {
  return boolean_attribute("disabled", is_disabled);
}

/**
 *
 */
export function for$(id) {
  return attribute("for", id);
}

/**
 * Associates the input with a form element located elsewhere in the document.
 */
export function form(id) {
  return attribute("form", id);
}

/**
 * The URL to use for form submission. This URL will override the [`"action"`](#action)
 * attribute on the form element itself, if present.
 *
 * The following input types support the `"formaction"` attribute:
 *
 * - `"image"`
 * - `"submit"`
 */
export function formaction(url) {
  return attribute("formaction", url);
}

/**
 * Entry list encoding type to use for form submission
 *
 * - `"image"`
 * - `"submit"`
 */
export function formenctype(encoding_type) {
  return attribute("formenctype", encoding_type);
}

/**
 * Variant to use for form submission
 *
 * - `"image"`
 * - `"submit"`
 */
export function formmethod(method) {
  return attribute("formmethod", method);
}

/**
 * Bypass form control validation for form submission
 *
 * - `"image"`
 * - `"submit"`
 */
export function formnovalidate(no_validate) {
  return boolean_attribute("formnovalidate", no_validate);
}

/**
 * Navigable for form submission
 *
 * - `"image"`
 * - `"submit"`
 */
export function formtarget(target) {
  return attribute("formtarget", target);
}

/**
 * List of autocomplete options
 *
 * The following input types support the `"list"` attribute:
 *
 * - `"color"`
 * - `"date"`
 * - `"datetime-local"`
 * - `"email"`
 * - `"month"`
 * - `"number"`
 * - `"range"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"time"`
 * - `"url"`
 * - `"week"`
 */
export function list(id) {
  return attribute("list", id);
}

/**
 * Constrain the maximum value of a form control. The exact syntax of this value
 * changes depending on the type of input, for example `"1"`, `"1979-12-31"`, and
 * `"06:00"` are all potentially valid values for the `"max"` attribute.
 *
 * The following input types support the `"max"` attribute:
 *
 * - `"date"`
 * - `"datetime-local"`
 * - `"month"`
 * - `"number"`
 * - `"range"`
 * - `"time"`
 * - `"week"`
 */
export function max(value) {
  return attribute("max", value);
}

/**
 * Maximum length of value
 *
 * The following input types support the `"maxlength"` attribute:
 *
 * - `"email"`
 * - `"password"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"url"`
 */
export function maxlength(length) {
  return attribute("maxlength", $int.to_string(length));
}

/**
 * Minimum value
 *
 * The following input types support the `"max"` attribute:
 *
 * - `"date"`
 * - `"datetime-local"`
 * - `"month"`
 * - `"number"`
 * - `"range"`
 * - `"time"`
 * - `"week"`
 */
export function min(value) {
  return attribute("min", value);
}

/**
 * Minimum length of value
 *
 * - `"email"`
 * - `"password"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"url"`
 */
export function minlength(length) {
  return attribute("minlength", $int.to_string(length));
}

/**
 * Whether an input or select may allow multiple values to be selected at once.
 *
 * The following input types support the `"multiple"` attribute:
 *
 * - `"email"`
 * - `"file"`
 */
export function multiple(allow_multiple) {
  return boolean_attribute("multiple", allow_multiple);
}

/**
 * Name of the element to use for form submission and in the form.elements API
 */
export function name(element_name) {
  return attribute("name", element_name);
}

/**
 * Pattern to be matched by the form control's value
 *
 * - `"email"`
 * - `"password"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"url"`
 */
export function pattern(regex) {
  return attribute("pattern", regex);
}

/**
 * User-visible label to be placed within the form control
 *
 * - `"email"`
 * - `"number"`
 * - `"password"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"url"`
 */
export function placeholder(text) {
  return attribute("placeholder", text);
}

/**
 * Targets a popover element to toggle, show, or hide
 *
 * The following input types support the `"popovertarget"` attribute:
 *
 * - `"button"`
 * - `"image"`
 * - `"reset"`
 * - `"submit"`
 */
export function popovertarget(id) {
  return attribute("popovertarget", id);
}

/**
 * Indicates whether a targeted popover element is to be toggled, shown, or hidden
 *
 * The following input types support the `"popovertarget"` attribute:
 *
 * - `"button"`
 * - `"image"`
 * - `"reset"`
 * - `"submit"`
 */
export function popovertargetaction(action) {
  return attribute("popovertargetaction", action);
}

/**
 * Whether to allow the value to be edited by the user
 *
 * - `"date"`
 * - `"datetime-local"`
 * - `"email"`
 * - `"month"`
 * - `"number"`
 * - `"password"`
 * - `"range"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"time"`
 * - `"url"`
 * - `"week"`
 */
export function readonly(is_readonly) {
  return boolean_attribute("readonly", is_readonly);
}

/**
 * Whether the control is required for form submission
 *
 * - `"checkbox"`
 * - `"date"`
 * - `"datetime-local"`
 * - `"email"`
 * - `"month"`
 * - `"number"`
 * - `"password"`
 * - `"radio"`
 * - `"range"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"time"`
 * - `"url"`
 * - `"week"`
 */
export function required(is_required) {
  return boolean_attribute("required", is_required);
}

/**
 * Controls whether or not a select's `<option>` is selected or not. Only one
 * option can be selected at a time, unless the [`"multiple"`](#multiple)
 * attribute is set on the select element.
 */
export function selected(is_selected) {
  return boolean_attribute("selected", is_selected);
}

/**
 * Size of the control
 *
 * The following input types support the `size` attribute:
 *
 * - `"email"`
 * - `"password"`
 * - `"search"`
 * - `"tel"`
 * - `"text"`
 * - `"url"`
 */
export function size(value) {
  return attribute("size", value);
}

/**
 * Granularity to be matched by the form control's value
 *
 * The following input types support the `"step"` attribute:
 *
 * - `"date"`
 * - `"datetime-local"`
 * - `"month"`
 * - `"number"`
 * - `"range"`
 * - `"time"`
 * - `"week"`
 */
export function step(value) {
  return attribute("step", value);
}

/**
 * Type of form control
 */
export function type_(control_type) {
  return attribute("type", control_type);
}

/**
 * Specifies the value of an input or form control. Using this attribute will
 * make sure the value is always in sync with your application's modelled, a
 * practice known as [_controlled inputs_](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
 *
 * If you'd like to let the DOM manage the value of an input but still set a
 * default value for users to see, use the [`default_value`](#default_value)
 * attribute instead.
 */
export function value(control_value) {
  return attribute("value", control_value);
}

/**
 * Set the default value of an input or form control. This is the value that will
 * be shown to users when the input is first rendered and included in the form
 * submission if the user does not change it.
 *
 * Just setting a default value and letting the DOM manage the state of an input
 * is known as using [_uncontrolled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).
 * Doing this means your application cannot set the value of an input after it
 * is modified without using an effect.
 */
export function default_value(control_value) {
  return attribute("virtual:defaultValue", control_value);
}

/**
 * Sets a pragma directive for a document. This is used in meta tags to define
 * behaviors the user agent should follow.
 */
export function http_equiv(value) {
  return attribute("http-equiv", value);
}

/**
 * Specifies the value of the meta element, which varies depending on the value
 * of the name or http-equiv attribute.
 */
export function content(value) {
  return attribute("content", value);
}

/**
 * Declares the character encoding used in the document. When used with a meta
 * element, this replaces the need for the `http_equiv("content-type")` attribute.
 */
export function charset(value) {
  return attribute("charset", value);
}

/**
 * Specifies the media types the resource applies to. This is commonly used with
 * link elements for stylesheets to determine when they should be loaded.
 */
export function media(query) {
  return attribute("media", query);
}

/**
 * Indicates that the media resource should automatically begin playing as soon
 * as it can do so without stopping. When not present, the media will not
 * automatically play until the user initiates playback.
 *
 * > **Note**: Lustre's runtime augments this attribute. Whenever it is toggled
 * > to true, the media will begin playing as if the element's `play()` method
 * > was called.
 */
export function autoplay(auto_play) {
  return boolean_attribute("autoplay", auto_play);
}

/**
 * When present, this attribute shows the browser's built-in control panel for the
 * media player, giving users control over playback, volume, seeking, and more.
 */
export function controls(show_controls) {
  return boolean_attribute("controls", show_controls);
}

/**
 * When present, this attribute indicates that the media should start over again
 * from the beginning when it reaches the end.
 */
export function loop(should_loop) {
  return boolean_attribute("loop", should_loop);
}

/**
 * When present, this attribute indicates that the audio output of the media element
 * should be initially silenced.
 */
export function muted(is_muted) {
  return boolean_attribute("muted", is_muted);
}

/**
 * Encourages the user agent to display video content within the element's
 * playback area rather than in a separate window or fullscreen, especially on
 * mobile devices.
 *
 * This attribute only acts as a *hint* to the user agent, and setting this to
 * false does not imply that the video will be played in fullscreen.
 */
export function playsinline(play_inline) {
  return boolean_attribute("playsinline", play_inline);
}

/**
 * Specifies an image to be shown while the video is downloading, or until the
 * user hits the play button.
 */
export function poster(url) {
  return attribute("poster", url);
}

/**
 * Provides a hint to the browser about what the author thinks will lead to the
 * best user experience. The following values are accepted:
 *
 * | Value      | Description                                                      |
 * |------------|------------------------------------------------------------------|
 * | "auto"     | Let's the user agent determine the best option                   |
 * | "metadata" | Hints to the user agent that it can fetch the metadata only.     |
 * | "none"     | Hints to the user agent that server traffic should be minimised. |
 */
export function preload(value) {
  return attribute("preload", value);
}

/**
 * Specifies the mode for creating a shadow root on a template. Valid values
 * include:
 *
 * | Value     | Description                                 |
 * |-----------|---------------------------------------------|
 * | "open"    | Shadow root's contents are accessible       |
 * | "closed"  | Shadow root's contents are not accessible   |
 *
 * > **Note**: if you are pre-rendering a Lustre component you must make sure this
 * > attribute matches the [`open_shadow_root`](./component.html#open_shadow_root)
 * > configuration - or `"closed"` if not explicitly set - to ensure the shadow
 * > root is created correctly.
 */
export function shadowrootmode(mode) {
  return attribute("shadowrootmode", mode);
}

/**
 * Indicates whether focus should be delegated to the shadow root when an element
 * in the shadow tree gains focus.
 */
export function shadowrootdelegatesfocus(delegates) {
  return boolean_attribute("shadowrootdelegatesfocus", delegates);
}

/**
 * Determines whether the shadow root can be cloned when the host element is
 * cloned.
 */
export function shadowrootclonable(clonable) {
  return boolean_attribute("shadowrootclonable", clonable);
}

/**
 * Controls whether the shadow root should be preserved during serialization
 * operations like copying to the clipboard or saving a page.
 */
export function shadowrootserializable(serializable) {
  return boolean_attribute("shadowrootserializable", serializable);
}

/**
 * A short, abbreviated description of the header cell's content provided as an
 * alternative label to use for the header cell when referencing the cell in other
 * contexts. Some user-agents, such as speech readers, may present this description
 * before the content itself.
 */
export function abbr(value) {
  return attribute("abbr", value);
}

/**
 * A non-negative integer value indicating how many columns the header cell spans
 * or extends. The default value is `1`. User agents dismiss values higher than
 * `1000` as incorrect, defaulting such values to `1`.
 */
export function colspan(value) {
  return attribute("colspan", $int.to_string(value));
}

/**
 * A list of space-separated strings corresponding to the id attributes of the
 * `<th>` elements that provide the headers for this header cell.
 */
export function headers(ids) {
  return attribute("headers", $string.join(ids, " "));
}

/**
 * A non-negative integer value indicating how many rows the header cell spans
 * or extends. The default value is `1`; if its value is set to `0`, the header
 * cell will extends to the end of the table grouping section, that the `<th>`
 * belongs to. Values higher than `65534` are clipped at `65534`.
 */
export function rowspan(value) {
  return attribute(
    "rowspan",
    (() => {
      let _pipe = value;
      let _pipe$1 = $int.max(_pipe, 0);
      let _pipe$2 = $int.min(_pipe$1, 65_534);
      return $int.to_string(_pipe$2);
    })(),
  );
}

/**
 * Specifies the number of consecutive columns a `<colgroup>` element spans. The
 * value must be a positive integer greater than zero.
 */
export function span(value) {
  return attribute("span", $int.to_string(value));
}

/**
 * The `scope` attribute specifies whether a header cell is a header for a row,
 * column, or group of rows or columns. The following values are accepted:
 *
 * The `scope` attribute is only valid on `<th>` elements.
 */
export function scope(value) {
  return attribute("scope", value);
}

/**
 * Add an `aria-*` attribute to an HTML element. The key will be prefixed by
 * `aria-`.
 */
export function aria(name, value) {
  return attribute("aria-" + name, value);
}

/**
 *
 */
export function role(name) {
  return attribute("role", name);
}

/**
 * The aria-activedescendant attribute identifies the currently active element
 * when focus is on a composite widget, combobox, textbox, group, or application.
 */
export function aria_activedescendant(id) {
  return aria("activedescendant", id);
}

/**
 * In ARIA live regions, the global aria-atomic attribute indicates whether
 * assistive technologies such as a screen reader will present all, or only parts
 * of, the changed region based on the change notifications defined by the
 * aria-relevant attribute.
 */
export function aria_atomic(value) {
  return aria(
    "atomic",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-autocomplete attribute indicates whether inputting text could trigger
 * display of one or more predictions of the user's intended value for a combobox,
 * searchbox, or textbox and specifies how predictions will be presented if they
 * are made.
 */
export function aria_autocomplete(value) {
  return aria("autocomplete", value);
}

/**
 * The global aria-braillelabel property defines a string value that labels the
 * current element, which is intended to be converted into Braille.
 */
export function aria_braillelabel(value) {
  return aria("braillelabel", value);
}

/**
 * The global aria-brailleroledescription attribute defines a human-readable,
 * author-localized abbreviated description for the role of an element intended
 * to be converted into Braille.
 */
export function aria_brailleroledescription(value) {
  return aria("brailleroledescription", value);
}

/**
 * Used in ARIA live regions, the global aria-busy state indicates an element is
 * being modified and that assistive technologies may want to wait until the
 * changes are complete before informing the user about the update.
 */
export function aria_busy(value) {
  return aria(
    "busy",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-checked attribute indicates the current "checked" state of checkboxes,
 * radio buttons, and other widgets.
 */
export function aria_checked(value) {
  return aria("checked", value);
}

/**
 * The aria-colcount attribute defines the total number of columns in a table,
 * grid, or treegrid when not all columns are present in the DOM.
 */
export function aria_colcount(value) {
  return aria("colcount", $int.to_string(value));
}

/**
 * The aria-colindex attribute defines an element's column index or position with
 * respect to the total number of columns within a table, grid, or treegrid.
 */
export function aria_colindex(value) {
  return aria("colindex", $int.to_string(value));
}

/**
 * The aria-colindextext attribute defines a human-readable text alternative of
 * the numeric aria-colindex.
 */
export function aria_colindextext(value) {
  return aria("colindextext", value);
}

/**
 * The aria-colspan attribute defines the number of columns spanned by a cell
 * or gridcell within a table, grid, or treegrid.
 */
export function aria_colspan(value) {
  return aria("colspan", $int.to_string(value));
}

/**
 * The global aria-controls property identifies the element (or elements) whose
 * contents or presence are controlled by the element on which this attribute is
 * set.
 */
export function aria_controls(value) {
  return aria("controls", value);
}

/**
 * A non-null aria-current state on an element indicates that this element represents
 * the current item within a container or set of related elements.
 */
export function aria_current(value) {
  return aria("current", value);
}

/**
 * The global aria-describedby attribute identifies the element (or elements)
 * that describes the element on which the attribute is set.
 */
export function aria_describedby(value) {
  return aria("describedby", value);
}

/**
 * The global aria-description attribute defines a string value that describes
 * or annotates the current element.
 */
export function aria_description(value) {
  return aria("description", value);
}

/**
 * The global aria-details attribute identifies the element (or elements) that
 * provide additional information related to the object.
 */
export function aria_details(value) {
  return aria("details", value);
}

/**
 * The aria-disabled state indicates that the element is perceivable but disabled,
 * so it is not editable or otherwise operable.
 */
export function aria_disabled(value) {
  return aria(
    "disabled",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-errormessage attribute on an object identifies the element that
 * provides an error message for that object.
 */
export function aria_errormessage(value) {
  return aria("errormessage", value);
}

/**
 * The aria-expanded attribute is set on an element to indicate if a control is
 * expanded or collapsed, and whether or not the controlled elements are displayed
 * or hidden.
 */
export function aria_expanded(value) {
  return aria(
    "expanded",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The global aria-flowto attribute identifies the next element (or elements) in
 * an alternate reading order of content. This allows assistive technology to
 * override the general default of reading in document source order at the user's
 * discretion.
 */
export function aria_flowto(value) {
  return aria("flowto", value);
}

/**
 * The aria-haspopup attribute indicates the availability and type of interactive
 * popup element that can be triggered by the element on which the attribute is
 * set.
 */
export function aria_haspopup(value) {
  return aria("haspopup", value);
}

/**
 * The aria-hidden state indicates whether the element is exposed to an accessibility
 * API.
 */
export function aria_hidden(value) {
  return aria(
    "hidden",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-invalid state indicates the entered value does not conform to the
 * format expected by the application.
 */
export function aria_invalid(value) {
  return aria("invalid", value);
}

/**
 * The global aria-keyshortcuts attribute indicates keyboard shortcuts that an
 * author has implemented to activate or give focus to an element.
 */
export function aria_keyshortcuts(value) {
  return aria("keyshortcuts", value);
}

/**
 * The aria-label attribute defines a string value that can be used to name an
 * element, as long as the element's role does not prohibit naming.
 */
export function aria_label(value) {
  return aria("label", value);
}

/**
 * The aria-labelledby attribute identifies the element (or elements) that labels
 * the element it is applied to.
 */
export function aria_labelledby(value) {
  return aria("labelledby", value);
}

/**
 * The aria-level attribute defines the hierarchical level of an element within
 * a structure.
 */
export function aria_level(value) {
  return aria("level", $int.to_string(value));
}

/**
 * The global aria-live attribute indicates that an element will be updated, and
 * describes the types of updates the user agents, assistive technologies, and
 * user can expect from the live region.
 */
export function aria_live(value) {
  return aria("live", value);
}

/**
 * The aria-modal attribute indicates whether an element is modal when displayed.
 */
export function aria_modal(value) {
  return aria(
    "modal",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-multiline attribute indicates whether a textbox accepts multiple
 * lines of input or only a single line.
 */
export function aria_multiline(value) {
  return aria(
    "multiline",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-multiselectable attribute indicates that the user may select more
 * than one item from the current selectable descendants.
 */
export function aria_multiselectable(value) {
  return aria(
    "multiselectable",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-orientation attribute indicates whether the element's orientation is
 * horizontal, vertical, or unknown/ambiguous.
 */
export function aria_orientation(value) {
  return aria("orientation", value);
}

/**
 * The aria-owns attribute identifies an element (or elements) in order to define
 * a visual, functional, or contextual relationship between a parent and its
 * child elements when the DOM hierarchy cannot be used to represent the relationship.
 */
export function aria_owns(value) {
  return aria("owns", value);
}

/**
 * The aria-placeholder attribute defines a short hint (a word or short phrase)
 * intended to help the user with data entry when a form control has no value.
 * The hint can be a sample value or a brief description of the expected format.
 */
export function aria_placeholder(value) {
  return aria("placeholder", value);
}

/**
 * The aria-posinset attribute defines an element's number or position in the
 * current set of listitems or treeitems when not all items are present in the
 * DOM.
 */
export function aria_posinset(value) {
  return aria("posinset", $int.to_string(value));
}

/**
 * The aria-pressed attribute indicates the current "pressed" state of a toggle
 * button.
 */
export function aria_pressed(value) {
  return aria("pressed", value);
}

/**
 * The aria-readonly attribute indicates that the element is not editable, but is
 * otherwise operable.
 */
export function aria_readonly(value) {
  return aria(
    "readonly",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * Used in ARIA live regions, the global aria-relevant attribute indicates what
 * notifications the user agent will trigger when the accessibility tree within
 * a live region is modified.
 */
export function aria_relevant(value) {
  return aria("relevant", value);
}

/**
 * The aria-required attribute indicates that user input is required on the element
 * before a form may be submitted.
 */
export function aria_required(value) {
  return aria(
    "required",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-roledescription attribute defines a human-readable, author-localised
 * description for the role of an element.
 */
export function aria_roledescription(value) {
  return aria("roledescription", value);
}

/**
 * The aria-rowcount attribute defines the total number of rows in a table,
 * grid, or treegrid.
 */
export function aria_rowcount(value) {
  return aria("rowcount", $int.to_string(value));
}

/**
 * The aria-rowindex attribute defines an element's position with respect to the
 * total number of rows within a table, grid, or treegrid.
 */
export function aria_rowindex(value) {
  return aria("rowindex", $int.to_string(value));
}

/**
 * The aria-rowindextext attribute defines a human-readable text alternative of
 * aria-rowindex.
 */
export function aria_rowindextext(value) {
  return aria("rowindextext", value);
}

/**
 * The aria-rowspan attribute defines the number of rows spanned by a cell or
 * gridcell within a table, grid, or treegrid.
 */
export function aria_rowspan(value) {
  return aria("rowspan", $int.to_string(value));
}

/**
 * The aria-selected attribute indicates the current "selected" state of various
 * widgets.
 */
export function aria_selected(value) {
  return aria(
    "selected",
    (() => {
      if (value) {
        return "true";
      } else {
        return "false";
      }
    })(),
  );
}

/**
 * The aria-setsize attribute defines the number of items in the current set of
 * listitems or treeitems when not all items in the set are present in the DOM.
 */
export function aria_setsize(value) {
  return aria("setsize", $int.to_string(value));
}

/**
 * The aria-sort attribute indicates if items in a table or grid are sorted in
 * ascending or descending order.
 */
export function aria_sort(value) {
  return aria("sort", value);
}

/**
 * The aria-valuemax attribute defines the maximum allowed value for a range
 * widget.
 */
export function aria_valuemax(value) {
  return aria("valuemax", value);
}

/**
 * The aria-valuemin attribute defines the minimum allowed value for a range
 * widget.
 */
export function aria_valuemin(value) {
  return aria("valuemin", value);
}

/**
 * The aria-valuenow attribute defines the current value for a range widget.
 */
export function aria_valuenow(value) {
  return aria("valuenow", value);
}

/**
 * The aria-valuetext attribute defines the human-readable text alternative of
 * aria-valuenow for a range widget.
 */
export function aria_valuetext(value) {
  return aria("valuetext", value);
}
