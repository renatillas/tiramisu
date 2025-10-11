-module(lustre@attribute).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/attribute.gleam").
-export([attribute/2, property/2, accesskey/1, autocapitalize/1, autocorrect/1, autofocus/1, class/1, none/0, classes/1, contenteditable/1, data/2, dir/1, draggable/1, enterkeyhint/1, hidden/1, id/1, inert/1, inputmode/1, is/1, itemid/1, itemprop/1, itemscope/1, itemtype/1, lang/1, nonce/1, popover/1, spellcheck/1, style/2, styles/1, tabindex/1, title/1, translate/1, writingsuggestions/1, href/1, target/1, download/1, ping/1, rel/1, hreflang/1, referrerpolicy/1, alt/1, src/1, srcset/1, sizes/1, crossorigin/1, usemap/1, ismap/1, width/1, height/1, decoding/1, loading/1, fetchpriority/1, accept_charset/1, action/1, enctype/1, method/1, novalidate/1, accept/1, alpha/1, autocomplete/1, checked/1, colorspace/1, dirname/1, disabled/1, for/1, form/1, formaction/1, formenctype/1, formmethod/1, formnovalidate/1, formtarget/1, list/1, max/1, maxlength/1, min/1, minlength/1, multiple/1, name/1, pattern/1, placeholder/1, popovertarget/1, popovertargetaction/1, readonly/1, required/1, selected/1, size/1, step/1, type_/1, value/1, default_value/1, http_equiv/1, content/1, charset/1, media/1, autoplay/1, controls/1, loop/1, muted/1, playsinline/1, poster/1, preload/1, shadowrootmode/1, shadowrootdelegatesfocus/1, shadowrootclonable/1, shadowrootserializable/1, abbr/1, colspan/1, headers/1, rowspan/1, span/1, scope/1, aria/2, role/1, aria_activedescendant/1, aria_atomic/1, aria_autocomplete/1, aria_braillelabel/1, aria_brailleroledescription/1, aria_busy/1, aria_checked/1, aria_colcount/1, aria_colindex/1, aria_colindextext/1, aria_colspan/1, aria_controls/1, aria_current/1, aria_describedby/1, aria_description/1, aria_details/1, aria_disabled/1, aria_errormessage/1, aria_expanded/1, aria_flowto/1, aria_haspopup/1, aria_hidden/1, aria_invalid/1, aria_keyshortcuts/1, aria_label/1, aria_labelledby/1, aria_level/1, aria_live/1, aria_modal/1, aria_multiline/1, aria_multiselectable/1, aria_orientation/1, aria_owns/1, aria_placeholder/1, aria_posinset/1, aria_pressed/1, aria_readonly/1, aria_relevant/1, aria_required/1, aria_roledescription/1, aria_rowcount/1, aria_rowindex/1, aria_rowindextext/1, aria_rowspan/1, aria_selected/1, aria_setsize/1, aria_sort/1, aria_valuemax/1, aria_valuemin/1, aria_valuenow/1, aria_valuetext/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/attribute.gleam", 34).
?DOC(
    " Create an HTML attribute. This is like saying `element.setAttribute(\"class\", \"wibble\")`\n"
    " in JavaScript. Attributes will be rendered when calling [`element.to_string`](./element.html#to_string).\n"
    "\n"
    " > **Note**: there is a subtle difference between attributes and properties. You\n"
    " > can read more about the implications of this\n"
    " > [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).\n"
).
-spec attribute(binary(), binary()) -> lustre@vdom@vattr:attribute(any()).
attribute(Name, Value) ->
    lustre@vdom@vattr:attribute(Name, Value).

-file("src/lustre/attribute.gleam", 53).
?DOC(
    " Create a DOM property. This is like saying `element.className = \"wibble\"` in\n"
    " JavaScript. Properties will be **not** be rendered when calling\n"
    " [`element.to_string`](./element.html#to_string).\n"
    "\n"
    " > **Note**: there is a subtle difference between attributes and properties. You\n"
    " > can read more about the implications of this\n"
    " > [here](https://github.com/lustre-labs/lustre/blob/main/pages/hints/attributes-vs-properties.md).\n"
).
-spec property(binary(), gleam@json:json()) -> lustre@vdom@vattr:attribute(any()).
property(Name, Value) ->
    lustre@vdom@vattr:property(Name, Value).

-file("src/lustre/attribute.gleam", 38).
-spec boolean_attribute(binary(), boolean()) -> lustre@vdom@vattr:attribute(any()).
boolean_attribute(Name, Value) ->
    case Value of
        true ->
            attribute(Name, <<""/utf8>>);

        false ->
            property(Name, gleam@json:bool(false))
    end.

-file("src/lustre/attribute.gleam", 79).
?DOC(
    " Defines a shortcut key to activate or focus the element. Multiple options\n"
    " may be provided as a set of space-separated characters that are exactly one\n"
    " code point each.\n"
    "\n"
    " The way to activate the access key depends on the browser and its platform:\n"
    "\n"
    " |         | Windows           | Linux               | Mac OS              |\n"
    " |---------|-------------------|---------------------|---------------------|\n"
    " | Firefox | Alt + Shift + key | Alt + Shift + key   | Ctrl + Option + key |\n"
    " | Chrome  | Alt + key         | Ctrl + Option + key | Ctrl + Option + key |\n"
    " | Safari  |                   |                     | Ctrl + Option + key |\n"
).
-spec accesskey(binary()) -> lustre@vdom@vattr:attribute(any()).
accesskey(Key) ->
    attribute(<<"accesskey"/utf8>>, Key).

-file("src/lustre/attribute.gleam", 112).
?DOC(
    " Controls whether text input is automatically capitalised. The following values\n"
    " are accepted:\n"
    "\n"
    " | Value        | Mode       |\n"
    " |--------------|------------|\n"
    " | \"\"           | default    |\n"
    " | \"none\"       | none       |\n"
    " | \"off\"        |            |\n"
    " | \"sentences\"  | sentences  |\n"
    " | \"on\"         |            |\n"
    " | \"words\"      | words      |\n"
    " | \"characters\" | characters |\n"
    "\n"
    " The autocapitalisation processing model is based on the following five modes:\n"
    "\n"
    " - **default**: The user agent and input method should make their own determination\n"
    "   of whether or not to enable autocapitalization.\n"
    "\n"
    " - **none**: No autocapitalisation should be applied (all letters should default\n"
    "   to lowercase).\n"
    "\n"
    " - **sentences**: The first letter of each sentence should default to a capital\n"
    "   letter; all other letters should default to lowercase.\n"
    "\n"
    " - **words**: The first letter of each word should default to a capital letter;\n"
    "   all other letters should default to lowercase.\n"
    "\n"
    " - **characters**: All letters should default to uppercase.\n"
).
-spec autocapitalize(binary()) -> lustre@vdom@vattr:attribute(any()).
autocapitalize(Value) ->
    attribute(<<"autocapitalize"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 122).
?DOC(
    " Controls whether the user agent may automatically correct mispelled words\n"
    " while typing. Whether or not spelling is corrected is left up to the user\n"
    " agent and may also depend on the user's settings.\n"
    "\n"
    " When disabled the user agent is **never** allowed to correct spelling.\n"
).
-spec autocorrect(boolean()) -> lustre@vdom@vattr:attribute(any()).
autocorrect(Enabled) ->
    boolean_attribute(<<"autocorrect"/utf8>>, Enabled).

-file("src/lustre/attribute.gleam", 133).
?DOC(
    " For server-rendered HTML, this attribute controls whether an element should\n"
    " be focused when the page first loads.\n"
    "\n"
    " > **Note**: Lustre's runtime augments that native behaviour of this attribute.\n"
    " > Whenever it is toggled true, the element will be automatically focused even\n"
    " > if it already exists in the DOM.\n"
).
-spec autofocus(boolean()) -> lustre@vdom@vattr:attribute(any()).
autofocus(Should_autofocus) ->
    boolean_attribute(<<"autofocus"/utf8>>, Should_autofocus).

-file("src/lustre/attribute.gleam", 148).
?DOC(
    " A class is a non-unique identifier for an element primarily used for styling\n"
    " purposes. You can provide multiple classes as a space-separated list and any\n"
    " style rules that apply to any of the classes will be applied to the element.\n"
    "\n"
    " To conditionally toggle classes on and off, you can use the [`classes`](#classes)\n"
    " function instead.\n"
    "\n"
    " > **Note**: unlike most attributes, multiple `class` attributes are merged\n"
    " > with any existing other classes on an element. Classes added _later_ in the\n"
    " > list will override classes added earlier.\n"
).
-spec class(binary()) -> lustre@vdom@vattr:attribute(any()).
class(Name) ->
    attribute(<<"class"/utf8>>, Name).

-file("src/lustre/attribute.gleam", 61).
?DOC(
    " Create an empty attribute. This is not added to the DOM and not rendered when\n"
    " calling [`element.to_string`](./element.html#to_string), but it is useful for\n"
    " _conditionally_ adding attributes to an element.\n"
).
-spec none() -> lustre@vdom@vattr:attribute(any()).
none() ->
    class(<<""/utf8>>).

-file("src/lustre/attribute.gleam", 165).
-spec do_classes(list({binary(), boolean()}), binary()) -> binary().
do_classes(Names, Class) ->
    case Names of
        [] ->
            Class;

        [{Name, true} | Rest] ->
            <<<<<<Class/binary, Name/binary>>/binary, " "/utf8>>/binary,
                (do_classes(Rest, Class))/binary>>;

        [{_, false} | Rest@1] ->
            do_classes(Rest@1, Class)
    end.

-file("src/lustre/attribute.gleam", 161).
?DOC(
    " A class is a non-unique identifier for an element primarily used for styling\n"
    " purposes. You can provide multiple classes as a space-separated list and any\n"
    " style rules that apply to any of the classes will be applied to the element.\n"
    " This function allows you to conditionally toggle classes on and off.\n"
    "\n"
    " > **Note**: unlike most attributes, multiple `class` attributes are merged\n"
    " > with any existing other classes on an element. Classes added _later_ in the\n"
    " > list will override classes added earlier.\n"
).
-spec classes(list({binary(), boolean()})) -> lustre@vdom@vattr:attribute(any()).
classes(Names) ->
    class(do_classes(Names, <<""/utf8>>)).

-file("src/lustre/attribute.gleam", 186).
?DOC(
    " Indicates whether the element's content is editable by the user, allowing them\n"
    " to modify the HTML content directly. The following values are accepted:\n"
    "\n"
    " | Value        | Description                                           |\n"
    " |--------------|-------------------------------------------------------|\n"
    " | \"true\"       | The element is editable.                              |\n"
    " | \"\"           |                                                       |\n"
    " | \"false\"      | The element is not editable.                          |\n"
    " | \"plain-text\" | The element is editable without rich text formatting. |\n"
    "\n"
    " > **Note**: setting the value to an empty string does *not* disable this\n"
    " > attribute, and is instead equivalent to setting it to `\"true\"`!\n"
).
-spec contenteditable(binary()) -> lustre@vdom@vattr:attribute(any()).
contenteditable(Is_editable) ->
    attribute(<<"contenteditable"/utf8>>, Is_editable).

-file("src/lustre/attribute.gleam", 195).
?DOC(
    " Add a `data-*` attribute to an HTML element. The key will be prefixed by\n"
    " `\"data-\"`, and accessible from JavaScript or in Gleam decoders under the\n"
    " path `element.dataset.key` where `key` is the key you provide to this\n"
    " function.\n"
).
-spec data(binary(), binary()) -> lustre@vdom@vattr:attribute(any()).
data(Key, Value) ->
    attribute(<<"data-"/utf8, Key/binary>>, Value).

-file("src/lustre/attribute.gleam", 213).
?DOC(
    " Specifies the text direction of the element's content. The following values\n"
    " are accepted:\n"
    "\n"
    " | Value  | Description                                                          |\n"
    " |--------|----------------------------------------------------------------------|\n"
    " | \"ltr\"  | The element's content is left-to-right.                              |\n"
    " | \"rtl\"  | The element's content is right-to-left.                              |\n"
    " | \"auto\" | The element's content direction is determined by the content itself. |\n"
    "\n"
    " > **Note**: the `\"auto\"` value should only be used as a last resort in cases\n"
    " > where the content's direction is truly unknown. The heuristic used by\n"
    " > browsers is naive and only considers the first character available that\n"
    " > indicates the direction.\n"
).
-spec dir(binary()) -> lustre@vdom@vattr:attribute(any()).
dir(Direction) ->
    attribute(<<"dir"/utf8>>, Direction).

-file("src/lustre/attribute.gleam", 220).
?DOC(
    " Indicates whether the element can be dragged as part of the HTML drag-and-drop\n"
    " API.\n"
).
-spec draggable(boolean()) -> lustre@vdom@vattr:attribute(any()).
draggable(Is_draggable) ->
    attribute(<<"draggable"/utf8>>, case Is_draggable of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 245).
?DOC(
    " Specifies what action label (or potentially icon) to present for the \"enter\"\n"
    " key on virtual keyboards such as mobile devices. The following values are\n"
    " accepted:\n"
    "\n"
    " | Value      | Example        |\n"
    " |------------|----------------|\n"
    " | \"enter\"    | \"return\", \"↵\"  |\n"
    " | \"done\"     | \"done\", \"✅\"   |\n"
    " | \"go\"       | \"go\"           |\n"
    " | \"next\"     | \"next\"         |\n"
    " | \"previous\" | \"return\"       |\n"
    " | \"search\"   | \"search\", \"🔍\" |\n"
    " | \"send\"     | \"send\"         |\n"
    "\n"
    " The examples listed are demonstrative and may not be the actual labels used\n"
    " by user agents. When unspecified or invalid, the user agent may use contextual\n"
    " information such as the type of an input to determine the label.\n"
).
-spec enterkeyhint(binary()) -> lustre@vdom@vattr:attribute(any()).
enterkeyhint(Value) ->
    attribute(<<"enterkeyhint"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 255).
?DOC(
    " Indicates whether the element is relevant to the page's current state. A\n"
    " hidden element is not visible to the user and is inaccessible to assistive\n"
    " technologies such as screen readers. This makes it unsuitable for simple\n"
    " presentation purposes, but it can be useful for example to render something\n"
    " that may be made visible later.\n"
).
-spec hidden(boolean()) -> lustre@vdom@vattr:attribute(any()).
hidden(Is_hidden) ->
    boolean_attribute(<<"hidden"/utf8>>, Is_hidden).

-file("src/lustre/attribute.gleam", 264).
?DOC(
    " The `\"id\"` attribute is used to uniquely identify a single element within a\n"
    " document. It can be used to reference the element in CSS with the selector\n"
    " `#id`, in JavaScript with `document.getElementById(\"id\")`, or by anchors on\n"
    " the same page with the URL `\"#id\"`.\n"
).
-spec id(binary()) -> lustre@vdom@vattr:attribute(any()).
id(Value) ->
    attribute(<<"id"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 274).
?DOC(
    " Marks the element as inert, meaning it is not currently interactive and does\n"
    " not receive user input. For sighted users, it's common to style inert elements\n"
    " in a way that makes them visually distinct from active elements, such as by\n"
    " greying them out: this can help avoid confusion for users who may not otherwise\n"
    " know the content they are looking at is inactive.\n"
).
-spec inert(boolean()) -> lustre@vdom@vattr:attribute(any()).
inert(Is_inert) ->
    boolean_attribute(<<"inert"/utf8>>, Is_inert).

-file("src/lustre/attribute.gleam", 295).
?DOC(
    " Hints to the user agent about what type of virtual keyboard to display when\n"
    " the user interacts with the element. The following values are accepted:\n"
    "\n"
    " | Value        | Description                                                   |\n"
    " |--------------|---------------------------------------------------------------|\n"
    " | \"none\"       | No virtual keyboard should be displayed.                      |\n"
    " | \"text\"       | A standard text input keyboard.                               |\n"
    " | \"decimal\"    | A numeric keyboard with locale-appropriate separator.         |\n"
    " | \"numeric\"    | A numeric keyboard.                                           |\n"
    " | \"tel\"        | A telephone keypad including \"#\" and \"*\".                     |\n"
    " | \"email\"      | A keyboard for entering email addresses including \"@\" and \".\" |\n"
    " | \"url\"        | A keyboard for entering URLs including \"/\" and \".\".           |\n"
    " | \"search\"     | A keyboard for entering search queries should be shown.       |\n"
    "\n"
    " The `\"none\"` value should only be used in cases where you are rendering a\n"
    " custom input method, otherwise the user will not be able to enter any text!\n"
).
-spec inputmode(binary()) -> lustre@vdom@vattr:attribute(any()).
inputmode(Value) ->
    attribute(<<"inputmode"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 302).
?DOC(
    " Specifies the [customised built-in element](https://html.spec.whatwg.org/#customized-built-in-element)\n"
    " to be used in place of the native element this attribute is applied to.\n"
).
-spec is(binary()) -> lustre@vdom@vattr:attribute(any()).
is(Value) ->
    attribute(<<"is"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 310).
?DOC(
    " Used as part of the [Microdata](https://schema.org/docs/gs.html) format to\n"
    " specify the global unique identifier of an item, for example books that are\n"
    " identifiable by their ISBN.\n"
).
-spec itemid(binary()) -> lustre@vdom@vattr:attribute(any()).
itemid(Id) ->
    attribute(<<"itemid"/utf8>>, Id).

-file("src/lustre/attribute.gleam", 318).
?DOC(
    " Used as part of the [Microdata](https://schema.org/docs/gs.html) format to\n"
    " specify that the content of the element is to be treated as a value of the\n"
    " given property name.\n"
).
-spec itemprop(binary()) -> lustre@vdom@vattr:attribute(any()).
itemprop(Name) ->
    attribute(<<"itemprop"/utf8>>, Name).

-file("src/lustre/attribute.gleam", 326).
?DOC(
    " Used as part of the [Microdata](https://schema.org/docs/gs.html) format to\n"
    " indicate that the element and its descendants form a single item of key-value\n"
    " data.\n"
).
-spec itemscope(boolean()) -> lustre@vdom@vattr:attribute(any()).
itemscope(Has_scope) ->
    boolean_attribute(<<"itemscope"/utf8>>, Has_scope).

-file("src/lustre/attribute.gleam", 335).
?DOC(
    " Used as part of the [Microdata](https://schema.org/docs/gs.html) format to\n"
    " specify the type of item being described. This is a URL that points to\n"
    " a schema containing the vocabulary used for an item's key-value pairs, such\n"
    " as a schema.org type.\n"
).
-spec itemtype(binary()) -> lustre@vdom@vattr:attribute(any()).
itemtype(Url) ->
    attribute(<<"itemtype"/utf8>>, Url).

-file("src/lustre/attribute.gleam", 346).
?DOC(
    " Specifies the language of the element's content and the language of any of\n"
    " this element's attributes that contain text. The `\"lang\"` attribute applies\n"
    " to the element itself and all of its descendants, unless overridden by\n"
    " another `\"lang\"` attribute on a descendant element.\n"
    "\n"
    " The value must be a valid [BCP 47 language tag](https://tools.ietf.org/html/bcp47).\n"
).
-spec lang(binary()) -> lustre@vdom@vattr:attribute(any()).
lang(Language) ->
    attribute(<<"lang"/utf8>>, Language).

-file("src/lustre/attribute.gleam", 353).
?DOC(
    " A cryptographic nonce used by CSP (Content Security Policy) to allow or\n"
    " deny the fetch of a given resource.\n"
).
-spec nonce(binary()) -> lustre@vdom@vattr:attribute(any()).
nonce(Value) ->
    attribute(<<"nonce"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 378).
?DOC(
    " Specifies that the element should be treated as a popover, rendering it in\n"
    " the top-layer above all other content when the popover is active. The following\n"
    " values are accepted:\n"
    "\n"
    " | Value        | Description                                    |\n"
    " |--------------|------------------------------------------------|\n"
    " | \"auto\"       | Closes other popovers when opened.             |\n"
    " | \"\"           |                                                |\n"
    " | \"manual\"     | Does not close other popovers when opened.     |\n"
    " | \"hint\"       | Closes only other \"hint\" popovers when opened. |\n"
    "\n"
    " All modes except `\"manual\"` support \"light dismiss\" letting the user close\n"
    " the popover by clicking outside of it, as well as respond to close requests\n"
    " letting the user dismiss a popover by pressing the \"escape\" key or by using\n"
    " the dismiss gesture on any assistive technology.\n"
    "\n"
    " Popovers can be triggered either programmatically through the `showPopover()`\n"
    " method, or by assigning an [`id`](#id) to the element and including the\n"
    " [`popovertarget`](#popovertarget) attribute on the element that should trigger\n"
    " the popover.\n"
).
-spec popover(binary()) -> lustre@vdom@vattr:attribute(any()).
popover(Value) ->
    attribute(<<"popover"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 386).
?DOC(
    " Indicates whether the element's content should be checked for spelling errors.\n"
    " This typically only applies to inputs and textareas, or elements that are\n"
    " [`contenteditable`](#contenteditable).\n"
).
-spec spellcheck(boolean()) -> lustre@vdom@vattr:attribute(any()).
spellcheck(Should_check) ->
    attribute(<<"spellcheck"/utf8>>, case Should_check of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 401).
?DOC(
    " Provide a single property name and value to be used as inline styles for the\n"
    " element. If either the property name or value is empty, this attribute will\n"
    " be ignored.\n"
    "\n"
    " > **Note**: unlike most attributes, multiple `style` attributes are merged\n"
    " > with any existing other styles on an element. Styles added _later_ in the\n"
    " > list will override styles added earlier.\n"
).
-spec style(binary(), binary()) -> lustre@vdom@vattr:attribute(any()).
style(Property, Value) ->
    case {Property, Value} of
        {<<""/utf8>>, _} ->
            class(<<""/utf8>>);

        {_, <<""/utf8>>} ->
            class(<<""/utf8>>);

        {_, _} ->
            attribute(
                <<"style"/utf8>>,
                <<<<<<Property/binary, ":"/utf8>>/binary, Value/binary>>/binary,
                    ";"/utf8>>
            )
    end.

-file("src/lustre/attribute.gleam", 419).
-spec do_styles(list({binary(), binary()}), binary()) -> binary().
do_styles(Properties, Styles) ->
    case Properties of
        [] ->
            Styles;

        [{<<""/utf8>>, _} | Rest] ->
            do_styles(Rest, Styles);

        [{_, <<""/utf8>>} | Rest] ->
            do_styles(Rest, Styles);

        [{Name, Value} | Rest@1] ->
            do_styles(
                Rest@1,
                <<<<<<<<Styles/binary, Name/binary>>/binary, ":"/utf8>>/binary,
                        Value/binary>>/binary,
                    ";"/utf8>>
            )
    end.

-file("src/lustre/attribute.gleam", 415).
?DOC(
    " Provide a list of property-value pairs to be used as inline styles for the\n"
    " element. Empty properties or values are omitted from the final style string.\n"
    "\n"
    " > **Note**: unlike most attributes, multiple `styles` attributes are merged\n"
    " > with any existing other styles on an element. Styles added _later_ in the\n"
    " > list will override styles added earlier.\n"
).
-spec styles(list({binary(), binary()})) -> lustre@vdom@vattr:attribute(any()).
styles(Properties) ->
    attribute(<<"style"/utf8>>, do_styles(Properties, <<""/utf8>>)).

-file("src/lustre/attribute.gleam", 447).
?DOC(
    " Specifies the tabbing order of the element. If an element is not typically\n"
    " focusable, such as a `<div>`, it will be made focusable when this attribute\n"
    " is set.\n"
    "\n"
    " Any integer value is accepted, but the following values are recommended:\n"
    "\n"
    " - `-1`: indicates the element may receive focus, but should not be sequentially\n"
    "   focusable. The user agent may choose to ignore this preference if, for\n"
    "   example, the user agent is a screen reader.\n"
    "\n"
    " - `0`: indicates the element may receive focus and should be placed in the\n"
    "   sequential focus order in the order it appears in the DOM.\n"
    "\n"
    " - any positive integer: indicates the element should be placed in the sequential\n"
    "   focus order relative to other elements with a positive tabindex.\n"
    "\n"
    " Values other than `0` and `-1` are generally not recommended as managing\n"
    " the relative order of focusable elements can be difficult and error-prone.\n"
).
-spec tabindex(integer()) -> lustre@vdom@vattr:attribute(any()).
tabindex(Index) ->
    attribute(<<"tabindex"/utf8>>, erlang:integer_to_binary(Index)).

-file("src/lustre/attribute.gleam", 459).
?DOC(
    " Annotate an element with additional information that may be suitable as a\n"
    " tooltip, such as a description of a link or image.\n"
    "\n"
    " It is **not** recommended to use the `title` attribute as a way of providing\n"
    " accessibility information to assistive technologies. User agents often do not\n"
    " expose the `title` attribute to keyboard-only users or touch devices, for\n"
    " example.\n"
).
-spec title(binary()) -> lustre@vdom@vattr:attribute(any()).
title(Text) ->
    attribute(<<"title"/utf8>>, Text).

-file("src/lustre/attribute.gleam", 481).
?DOC(
    " Controls whether an element's content may be translated by the user agent\n"
    " when the page is localised. This includes both the element's text content\n"
    " and some of its attributes:\n"
    "\n"
    " | Attribute   | Element(s)                                 |\n"
    " |-------------|--------------------------------------------|\n"
    " | abbr        | th                                         |\n"
    " | alt         | area, img, input                           |\n"
    " | content     | meta                                       |\n"
    " | download    | a, area                                    |\n"
    " | label       | optgroup, option, track                    |\n"
    " | lang        | *                                          |\n"
    " | placeholder | input, textarea                            |\n"
    " | srcdoc      | iframe                                     |\n"
    " | title       | *                                          |\n"
    " | style       | *                                          |\n"
    " | value       | input (with type=\"button\" or type=\"reset\") |\n"
).
-spec translate(boolean()) -> lustre@vdom@vattr:attribute(any()).
translate(Should_translate) ->
    attribute(<<"translate"/utf8>>, case Should_translate of
            true ->
                <<"yes"/utf8>>;

            false ->
                <<"no"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 490).
?DOC(" Indicates if writing suggestions should be enabled for this element.\n").
-spec writingsuggestions(boolean()) -> lustre@vdom@vattr:attribute(any()).
writingsuggestions(Enabled) ->
    attribute(<<"writingsuggestions"/utf8>>, case Enabled of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 502).
?DOC(
    " Specifies the URL of a linked resource. This attribute can be used on various\n"
    " elements to create hyperlinks or to load resources.\n"
).
-spec href(binary()) -> lustre@vdom@vattr:attribute(any()).
href(Url) ->
    attribute(<<"href"/utf8>>, Url).

-file("src/lustre/attribute.gleam", 520).
?DOC(
    " Specifies where to display the linked resource or where to open the link.\n"
    " The following values are accepted:\n"
    "\n"
    " | Value     | Description                                             |\n"
    " |-----------|---------------------------------------------------------|\n"
    " | \"_self\"   | Open in the same frame/window (default)                 |\n"
    " | \"_blank\"  | Open in a new window or tab                             |\n"
    " | \"_parent\" | Open in the parent frame                                |\n"
    " | \"_top\"    | Open in the full body of the window                     |\n"
    " | framename | Open in a named frame                                   |\n"
    "\n"
    " > **Note**: consider against using `\"_blank\"` for links to external sites as it\n"
    " > removes user control over their browsing experience.\n"
).
-spec target(binary()) -> lustre@vdom@vattr:attribute(any()).
target(Value) ->
    attribute(<<"target"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 527).
?DOC(
    " Indicates that the linked resource should be downloaded rather than displayed.\n"
    " When provided with a value, it suggests a filename for the downloaded file.\n"
).
-spec download(binary()) -> lustre@vdom@vattr:attribute(any()).
download(Filename) ->
    attribute(<<"download"/utf8>>, Filename).

-file("src/lustre/attribute.gleam", 535).
?DOC(
    " Provides a space-separated list of URLs that will be notified if the user\n"
    " follows the hyperlink. These URLs will receive POST requests with bodies\n"
    " of type `ping/1.0`.\n"
).
-spec ping(list(binary())) -> lustre@vdom@vattr:attribute(any()).
ping(Urls) ->
    attribute(<<"ping"/utf8>>, gleam@string:join(Urls, <<" "/utf8>>)).

-file("src/lustre/attribute.gleam", 542).
?DOC(
    " Specifies the relationship between the current document and the linked resource.\n"
    " Multiple relationship values can be provided as a space-separated list.\n"
).
-spec rel(binary()) -> lustre@vdom@vattr:attribute(any()).
rel(Value) ->
    attribute(<<"rel"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 549).
?DOC(
    " Specifies the language of the linked resource. The value must be a valid\n"
    " [BCP 47 language tag](https://tools.ietf.org/html/bcp47).\n"
).
-spec hreflang(binary()) -> lustre@vdom@vattr:attribute(any()).
hreflang(Language) ->
    attribute(<<"hreflang"/utf8>>, Language).

-file("src/lustre/attribute.gleam", 567).
?DOC(
    " Specifies the referrer policy for fetches initiated by the element. The\n"
    " following values are accepted:\n"
    "\n"
    " | Value                              | Description                                           |\n"
    " |-----------------------------------|--------------------------------------------------------|\n"
    " | \"no-referrer\"                     | No Referer header is sent                              |\n"
    " | \"no-referrer-when-downgrade\"      | Only send Referer for same-origin or more secure       |\n"
    " | \"origin\"                          | Send only the origin part of the URL                   |\n"
    " | \"origin-when-cross-origin\"        | Full URL for same-origin, origin only for cross-origin |\n"
    " | \"same-origin\"                     | Only send Referer for same-origin requests             |\n"
    " | \"strict-origin\"                   | Like origin, but only to equally secure destinations   |\n"
    " | \"strict-origin-when-cross-origin\" | Default policy with varying levels of restriction      |\n"
    " | \"unsafe-url\"                      | Always send the full URL                               |\n"
).
-spec referrerpolicy(binary()) -> lustre@vdom@vattr:attribute(any()).
referrerpolicy(Value) ->
    attribute(<<"referrerpolicy"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 577).
?DOC(
    " Specifies text that should be displayed when the image cannot be rendered.\n"
    " This attribute is essential for accessibility, providing context about the\n"
    " image to users who cannot see it, including those using screen readers.\n"
).
-spec alt(binary()) -> lustre@vdom@vattr:attribute(any()).
alt(Text) ->
    attribute(<<"alt"/utf8>>, Text).

-file("src/lustre/attribute.gleam", 583).
?DOC(" Specifies the URL of an image or resource to be used.\n").
-spec src(binary()) -> lustre@vdom@vattr:attribute(any()).
src(Url) ->
    attribute(<<"src"/utf8>>, Url).

-file("src/lustre/attribute.gleam", 591).
?DOC(
    " Specifies a set of image sources for different display scenarios. This allows\n"
    " browsers to choose the most appropriate image based on factors like screen\n"
    " resolution and viewport size.\n"
).
-spec srcset(binary()) -> lustre@vdom@vattr:attribute(any()).
srcset(Sources) ->
    attribute(<<"srcset"/utf8>>, Sources).

-file("src/lustre/attribute.gleam", 598).
?DOC(
    " Used with `srcset` to define the size of images in different layout scenarios.\n"
    " Helps the browser select the most appropriate image source.\n"
).
-spec sizes(binary()) -> lustre@vdom@vattr:attribute(any()).
sizes(Value) ->
    attribute(<<"sizes"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 605).
?DOC(
    " Configures the CORS (Cross-Origin Resource Sharing) settings for the element.\n"
    " Valid values are \"anonymous\" and \"use-credentials\".\n"
).
-spec crossorigin(binary()) -> lustre@vdom@vattr:attribute(any()).
crossorigin(Value) ->
    attribute(<<"crossorigin"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 611).
?DOC(" Specifies the name of an image map to be used with the image.\n").
-spec usemap(binary()) -> lustre@vdom@vattr:attribute(any()).
usemap(Value) ->
    attribute(<<"usemap"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 618).
?DOC(
    " Indicates that the image is a server-side image map. When a user clicks on the\n"
    " image, the coordinates of the click are sent to the server.\n"
).
-spec ismap(boolean()) -> lustre@vdom@vattr:attribute(any()).
ismap(Is_map) ->
    boolean_attribute(<<"ismap"/utf8>>, Is_map).

-file("src/lustre/attribute.gleam", 624).
?DOC(" Specifies the width of the element in pixels.\n").
-spec width(integer()) -> lustre@vdom@vattr:attribute(any()).
width(Value) ->
    attribute(<<"width"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 630).
?DOC(" Specifies the height of the element in pixels.\n").
-spec height(integer()) -> lustre@vdom@vattr:attribute(any()).
height(Value) ->
    attribute(<<"height"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 637).
?DOC(
    " Provides a hint about how the image should be decoded. Valid values are\n"
    " \"sync\", \"async\", and \"auto\".\n"
).
-spec decoding(binary()) -> lustre@vdom@vattr:attribute(any()).
decoding(Value) ->
    attribute(<<"decoding"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 644).
?DOC(
    " Indicates how the browser should load the image. Valid values are \"eager\"\n"
    " (load immediately) and \"lazy\" (defer loading until needed).\n"
).
-spec loading(binary()) -> lustre@vdom@vattr:attribute(any()).
loading(Value) ->
    attribute(<<"loading"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 651).
?DOC(
    " Sets the priority for fetches initiated by the element. Valid values are\n"
    " \"high\", \"low\", and \"auto\".\n"
).
-spec fetchpriority(binary()) -> lustre@vdom@vattr:attribute(any()).
fetchpriority(Value) ->
    attribute(<<"fetchpriority"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 661).
?DOC(
    " Specifies the character encodings to be used for form submission. This allows\n"
    " servers to know how to interpret the form data. Multiple encodings can be\n"
    " specified as a space-separated list.\n"
).
-spec accept_charset(binary()) -> lustre@vdom@vattr:attribute(any()).
accept_charset(Charsets) ->
    attribute(<<"accept-charset"/utf8>>, Charsets).

-file("src/lustre/attribute.gleam", 668).
?DOC(
    " Specifies the URL to which the form's data should be sent when submitted.\n"
    " This can be overridden by formaction attributes on submit buttons.\n"
).
-spec action(binary()) -> lustre@vdom@vattr:attribute(any()).
action(Url) ->
    attribute(<<"action"/utf8>>, Url).

-file("src/lustre/attribute.gleam", 681).
?DOC(
    " Specifies how form data should be encoded before sending it to the server.\n"
    " Valid values include:\n"
    "\n"
    " | Value                             | Description                           |\n"
    " |-----------------------------------|---------------------------------------|\n"
    " | \"application/x-www-form-urlencoded\" | Default encoding (spaces as +, etc.)  |\n"
    " | \"multipart/form-data\"               | Required for file uploads             |\n"
    " | \"text/plain\"                        | Simple encoding with minimal escaping  |\n"
).
-spec enctype(binary()) -> lustre@vdom@vattr:attribute(any()).
enctype(Encoding_type) ->
    attribute(<<"enctype"/utf8>>, Encoding_type).

-file("src/lustre/attribute.gleam", 693).
?DOC(
    " Specifies the HTTP method to use when submitting the form. Common values are:\n"
    "\n"
    " | Value    | Description                                              |\n"
    " |----------|----------------------------------------------------------|\n"
    " | \"get\"    | Appends form data to URL (default)                       |\n"
    " | \"post\"   | Sends form data in the body of the HTTP request          |\n"
    " | \"dialog\" | Closes a dialog if the form is inside one                |\n"
).
-spec method(binary()) -> lustre@vdom@vattr:attribute(any()).
method(Http_method) ->
    attribute(<<"method"/utf8>>, Http_method).

-file("src/lustre/attribute.gleam", 700).
?DOC(
    " When present, indicates that the form should not be validated when submitted.\n"
    " This allows submission of forms with invalid or incomplete data.\n"
).
-spec novalidate(boolean()) -> lustre@vdom@vattr:attribute(any()).
novalidate(Disable_validation) ->
    boolean_attribute(<<"novalidate"/utf8>>, Disable_validation).

-file("src/lustre/attribute.gleam", 725).
?DOC(
    " A hint for the user agent about what file types are expected to be submitted.\n"
    " The following values are accepted:\n"
    "\n"
    " | Value     | Description                                          |\n"
    " |-----------|------------------------------------------------------|\n"
    " | \"audio/*\" | Any audio file type.                                 |\n"
    " | \"video/*\" | Any video file type.                                 |\n"
    " | \"image/*\" | Any image file type.                                 |\n"
    " | mime/type | A complete MIME type, without additional parameters. |\n"
    " | .ext      | Indicates any file with the given extension.         |\n"
    "\n"
    " The following input types support the `\"accept\"` attribute:\n"
    "\n"
    " - `\"file\"`\n"
    "\n"
    " > **Note**: the `\"accept\"` attribute is a *hint* to the user agent and does\n"
    " > not guarantee that the user will only be able to select files of the specified\n"
    " > type.\n"
).
-spec accept(list(binary())) -> lustre@vdom@vattr:attribute(any()).
accept(Values) ->
    attribute(<<"accept"/utf8>>, gleam@string:join(Values, <<","/utf8>>)).

-file("src/lustre/attribute.gleam", 736).
?DOC(
    " Allow a colour's alpha component to be manipulated, allowing the user to\n"
    " select a colour with transparency.\n"
    "\n"
    " The following input types support the `\"alpha\"` attribute:\n"
    "\n"
    " - `\"color\"`\n"
).
-spec alpha(boolean()) -> lustre@vdom@vattr:attribute(any()).
alpha(Allowed) ->
    boolean_attribute(<<"alpha"/utf8>>, Allowed).

-file("src/lustre/attribute.gleam", 781).
?DOC(
    " A hint for the user agent to automatically fill the value of the input with\n"
    " an appropriate value. The format for the `\"autocomplete\"` attribute is a\n"
    " space-separated ordered list of optional tokens:\n"
    "\n"
    "     \"section-* (shipping | billing) [...fields] webauthn\"\n"
    "\n"
    " - `section-*`: used to disambiguate between two fields with otherwise identical\n"
    "   autocomplete values. The `*` is replaced with a string that identifies the\n"
    "   section of the form.\n"
    "\n"
    " - `shipping | billing`: indicates the field is related to shipping or billing\n"
    "   address or contact information.\n"
    "\n"
    " - `[...fields]`: a space-separated list of field names that are relevant to\n"
    "   the input, for example `\"email\"`, `\"name family-name\"`, or `\"home tel\"`.\n"
    "\n"
    " - `webauthn`: indicates the field can be automatically filled with a WebAuthn\n"
    "   passkey.\n"
    "\n"
    " In addition, the value may instead be `\"off\"` to disable autocomplete for the\n"
    " input, or `\"on\"` to let the user agent decide based on context what values\n"
    " are appropriate.\n"
    "\n"
    " The following input types support the `\"autocomplete\"` attribute:\n"
    "\n"
    " - `\"color\"`\n"
    " - `\"date\"`\n"
    " - `\"datetime-local\"`\n"
    " - `\"email\"`\n"
    " - `\"hidden\"`\n"
    " - `\"month\"`\n"
    " - `\"number\"`\n"
    " - `\"password\"`\n"
    " - `\"range\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"time\"`\n"
    " - `\"url\"`\n"
    " - `\"week\"`\n"
).
-spec autocomplete(binary()) -> lustre@vdom@vattr:attribute(any()).
autocomplete(Value) ->
    attribute(<<"autocomplete"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 795).
?DOC(
    " Whether the control is checked or not. When participating in a form, the\n"
    " value of the input is included in the form submission if it is checked. For\n"
    " checkboxes that do not have a value, the value of the input is `\"on\"` when\n"
    " checked.\n"
    "\n"
    " The following input types support the `\"checked\"` attribute:\n"
    "\n"
    " - `\"checkbox\"`\n"
    " - `\"radio\"`\n"
).
-spec checked(boolean()) -> lustre@vdom@vattr:attribute(any()).
checked(Is_checked) ->
    boolean_attribute(<<"checked"/utf8>>, Is_checked).

-file("src/lustre/attribute.gleam", 814).
?DOC(
    " The color space of the serialised CSS color. It also hints to user agents\n"
    " about what kind of interface to present to the user for selecting a color.\n"
    " The following values are accepted:\n"
    "\n"
    " - `\"limited-srgb\"`: The CSS color is converted to the 'srgb' color space and\n"
    "   limited to 8-bits per component, e.g., `\"#123456\"` or\n"
    "   `\"color(srgb 0 1 0 / 0.5)\"`.\n"
    "\n"
    " - `\"display-p3\"`: The CSS color is converted to the 'display-p3' color space,\n"
    "   e.g., `\"color(display-p3 1.84 -0.19 0.72 / 0.6)\"`.\n"
    "\n"
    " The following input types support the `\"colorspace\"` attribute:\n"
    "\n"
    " - `\"color\"`\n"
).
-spec colorspace(binary()) -> lustre@vdom@vattr:attribute(any()).
colorspace(Value) ->
    attribute(<<"colorspace"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 832).
?DOC(
    " The name of the field included in a form that indicates the direcionality of\n"
    " the user's input.\n"
    "\n"
    " The following input types support the `\"dirname\"` attribute:\n"
    "\n"
    " - `\"email\"`\n"
    " - `\"hidden\"`\n"
    " - `\"password\"`\n"
    " - `\"search\"`\n"
    " - `\"submit\"\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"url\"`\n"
).
-spec dirname(binary()) -> lustre@vdom@vattr:attribute(any()).
dirname(Direction) ->
    attribute(<<"dirname"/utf8>>, Direction).

-file("src/lustre/attribute.gleam", 839).
?DOC(
    " Controls whether or not the input is disabled. Disabled inputs are not\n"
    " validated during form submission and are not interactive.\n"
).
-spec disabled(boolean()) -> lustre@vdom@vattr:attribute(any()).
disabled(Is_disabled) ->
    boolean_attribute(<<"disabled"/utf8>>, Is_disabled).

-file("src/lustre/attribute.gleam", 845).
?DOC("\n").
-spec for(binary()) -> lustre@vdom@vattr:attribute(any()).
for(Id) ->
    attribute(<<"for"/utf8>>, Id).

-file("src/lustre/attribute.gleam", 851).
?DOC(" Associates the input with a form element located elsewhere in the document.\n").
-spec form(binary()) -> lustre@vdom@vattr:attribute(any()).
form(Id) ->
    attribute(<<"form"/utf8>>, Id).

-file("src/lustre/attribute.gleam", 863).
?DOC(
    " The URL to use for form submission. This URL will override the [`\"action\"`](#action)\n"
    " attribute on the form element itself, if present.\n"
    "\n"
    " The following input types support the `\"formaction\"` attribute:\n"
    "\n"
    " - `\"image\"`\n"
    " - `\"submit\"`\n"
).
-spec formaction(binary()) -> lustre@vdom@vattr:attribute(any()).
formaction(Url) ->
    attribute(<<"formaction"/utf8>>, Url).

-file("src/lustre/attribute.gleam", 872).
?DOC(
    " Entry list encoding type to use for form submission\n"
    "\n"
    " - `\"image\"`\n"
    " - `\"submit\"`\n"
).
-spec formenctype(binary()) -> lustre@vdom@vattr:attribute(any()).
formenctype(Encoding_type) ->
    attribute(<<"formenctype"/utf8>>, Encoding_type).

-file("src/lustre/attribute.gleam", 881).
?DOC(
    " Variant to use for form submission\n"
    "\n"
    " - `\"image\"`\n"
    " - `\"submit\"`\n"
).
-spec formmethod(binary()) -> lustre@vdom@vattr:attribute(any()).
formmethod(Method) ->
    attribute(<<"formmethod"/utf8>>, Method).

-file("src/lustre/attribute.gleam", 890).
?DOC(
    " Bypass form control validation for form submission\n"
    "\n"
    " - `\"image\"`\n"
    " - `\"submit\"`\n"
).
-spec formnovalidate(boolean()) -> lustre@vdom@vattr:attribute(any()).
formnovalidate(No_validate) ->
    boolean_attribute(<<"formnovalidate"/utf8>>, No_validate).

-file("src/lustre/attribute.gleam", 899).
?DOC(
    " Navigable for form submission\n"
    "\n"
    " - `\"image\"`\n"
    " - `\"submit\"`\n"
).
-spec formtarget(binary()) -> lustre@vdom@vattr:attribute(any()).
formtarget(Target) ->
    attribute(<<"formtarget"/utf8>>, Target).

-file("src/lustre/attribute.gleam", 921).
?DOC(
    " List of autocomplete options\n"
    "\n"
    " The following input types support the `\"list\"` attribute:\n"
    "\n"
    " - `\"color\"`\n"
    " - `\"date\"`\n"
    " - `\"datetime-local\"`\n"
    " - `\"email\"`\n"
    " - `\"month\"`\n"
    " - `\"number\"`\n"
    " - `\"range\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"time\"`\n"
    " - `\"url\"`\n"
    " - `\"week\"`\n"
).
-spec list(binary()) -> lustre@vdom@vattr:attribute(any()).
list(Id) ->
    attribute(<<"list"/utf8>>, Id).

-file("src/lustre/attribute.gleam", 939).
?DOC(
    " Constrain the maximum value of a form control. The exact syntax of this value\n"
    " changes depending on the type of input, for example `\"1\"`, `\"1979-12-31\"`, and\n"
    " `\"06:00\"` are all potentially valid values for the `\"max\"` attribute.\n"
    "\n"
    " The following input types support the `\"max\"` attribute:\n"
    "\n"
    " - `\"date\"`\n"
    " - `\"datetime-local\"`\n"
    " - `\"month\"`\n"
    " - `\"number\"`\n"
    " - `\"range\"`\n"
    " - `\"time\"`\n"
    " - `\"week\"`\n"
).
-spec max(binary()) -> lustre@vdom@vattr:attribute(any()).
max(Value) ->
    attribute(<<"max"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 954).
?DOC(
    " Maximum length of value\n"
    "\n"
    " The following input types support the `\"maxlength\"` attribute:\n"
    "\n"
    " - `\"email\"`\n"
    " - `\"password\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"url\"`\n"
).
-spec maxlength(integer()) -> lustre@vdom@vattr:attribute(any()).
maxlength(Length) ->
    attribute(<<"maxlength"/utf8>>, erlang:integer_to_binary(Length)).

-file("src/lustre/attribute.gleam", 970).
?DOC(
    " Minimum value\n"
    "\n"
    " The following input types support the `\"max\"` attribute:\n"
    "\n"
    " - `\"date\"`\n"
    " - `\"datetime-local\"`\n"
    " - `\"month\"`\n"
    " - `\"number\"`\n"
    " - `\"range\"`\n"
    " - `\"time\"`\n"
    " - `\"week\"`\n"
).
-spec min(binary()) -> lustre@vdom@vattr:attribute(any()).
min(Value) ->
    attribute(<<"min"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 983).
?DOC(
    " Minimum length of value\n"
    "\n"
    " - `\"email\"`\n"
    " - `\"password\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"url\"`\n"
).
-spec minlength(integer()) -> lustre@vdom@vattr:attribute(any()).
minlength(Length) ->
    attribute(<<"minlength"/utf8>>, erlang:integer_to_binary(Length)).

-file("src/lustre/attribute.gleam", 994).
?DOC(
    " Whether an input or select may allow multiple values to be selected at once.\n"
    "\n"
    " The following input types support the `\"multiple\"` attribute:\n"
    "\n"
    " - `\"email\"`\n"
    " - `\"file\"`\n"
).
-spec multiple(boolean()) -> lustre@vdom@vattr:attribute(any()).
multiple(Allow_multiple) ->
    boolean_attribute(<<"multiple"/utf8>>, Allow_multiple).

-file("src/lustre/attribute.gleam", 1000).
?DOC(" Name of the element to use for form submission and in the form.elements API\n").
-spec name(binary()) -> lustre@vdom@vattr:attribute(any()).
name(Element_name) ->
    attribute(<<"name"/utf8>>, Element_name).

-file("src/lustre/attribute.gleam", 1013).
?DOC(
    " Pattern to be matched by the form control's value\n"
    "\n"
    " - `\"email\"`\n"
    " - `\"password\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"url\"`\n"
).
-spec pattern(binary()) -> lustre@vdom@vattr:attribute(any()).
pattern(Regex) ->
    attribute(<<"pattern"/utf8>>, Regex).

-file("src/lustre/attribute.gleam", 1027).
?DOC(
    " User-visible label to be placed within the form control\n"
    "\n"
    " - `\"email\"`\n"
    " - `\"number\"`\n"
    " - `\"password\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"url\"`\n"
).
-spec placeholder(binary()) -> lustre@vdom@vattr:attribute(any()).
placeholder(Text) ->
    attribute(<<"placeholder"/utf8>>, Text).

-file("src/lustre/attribute.gleam", 1040).
?DOC(
    " Targets a popover element to toggle, show, or hide\n"
    "\n"
    " The following input types support the `\"popovertarget\"` attribute:\n"
    "\n"
    " - `\"button\"`\n"
    " - `\"image\"`\n"
    " - `\"reset\"`\n"
    " - `\"submit\"`\n"
).
-spec popovertarget(binary()) -> lustre@vdom@vattr:attribute(any()).
popovertarget(Id) ->
    attribute(<<"popovertarget"/utf8>>, Id).

-file("src/lustre/attribute.gleam", 1053).
?DOC(
    " Indicates whether a targeted popover element is to be toggled, shown, or hidden\n"
    "\n"
    " The following input types support the `\"popovertarget\"` attribute:\n"
    "\n"
    " - `\"button\"`\n"
    " - `\"image\"`\n"
    " - `\"reset\"`\n"
    " - `\"submit\"`\n"
).
-spec popovertargetaction(binary()) -> lustre@vdom@vattr:attribute(any()).
popovertargetaction(Action) ->
    attribute(<<"popovertargetaction"/utf8>>, Action).

-file("src/lustre/attribute.gleam", 1073).
?DOC(
    " Whether to allow the value to be edited by the user\n"
    "\n"
    " - `\"date\"`\n"
    " - `\"datetime-local\"`\n"
    " - `\"email\"`\n"
    " - `\"month\"`\n"
    " - `\"number\"`\n"
    " - `\"password\"`\n"
    " - `\"range\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"time\"`\n"
    " - `\"url\"`\n"
    " - `\"week\"`\n"
).
-spec readonly(boolean()) -> lustre@vdom@vattr:attribute(any()).
readonly(Is_readonly) ->
    boolean_attribute(<<"readonly"/utf8>>, Is_readonly).

-file("src/lustre/attribute.gleam", 1095).
?DOC(
    " Whether the control is required for form submission\n"
    "\n"
    " - `\"checkbox\"`\n"
    " - `\"date\"`\n"
    " - `\"datetime-local\"`\n"
    " - `\"email\"`\n"
    " - `\"month\"`\n"
    " - `\"number\"`\n"
    " - `\"password\"`\n"
    " - `\"radio\"`\n"
    " - `\"range\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"time\"`\n"
    " - `\"url\"`\n"
    " - `\"week\"`\n"
).
-spec required(boolean()) -> lustre@vdom@vattr:attribute(any()).
required(Is_required) ->
    boolean_attribute(<<"required"/utf8>>, Is_required).

-file("src/lustre/attribute.gleam", 1103).
?DOC(
    " Controls whether or not a select's `<option>` is selected or not. Only one\n"
    " option can be selected at a time, unless the [`\"multiple\"`](#multiple)\n"
    " attribute is set on the select element.\n"
).
-spec selected(boolean()) -> lustre@vdom@vattr:attribute(any()).
selected(Is_selected) ->
    boolean_attribute(<<"selected"/utf8>>, Is_selected).

-file("src/lustre/attribute.gleam", 1118).
?DOC(
    " Size of the control\n"
    "\n"
    " The following input types support the `size` attribute:\n"
    "\n"
    " - `\"email\"`\n"
    " - `\"password\"`\n"
    " - `\"search\"`\n"
    " - `\"tel\"`\n"
    " - `\"text\"`\n"
    " - `\"url\"`\n"
).
-spec size(binary()) -> lustre@vdom@vattr:attribute(any()).
size(Value) ->
    attribute(<<"size"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1134).
?DOC(
    " Granularity to be matched by the form control's value\n"
    "\n"
    " The following input types support the `\"step\"` attribute:\n"
    "\n"
    " - `\"date\"`\n"
    " - `\"datetime-local\"`\n"
    " - `\"month\"`\n"
    " - `\"number\"`\n"
    " - `\"range\"`\n"
    " - `\"time\"`\n"
    " - `\"week\"`\n"
).
-spec step(binary()) -> lustre@vdom@vattr:attribute(any()).
step(Value) ->
    attribute(<<"step"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1140).
?DOC(" Type of form control\n").
-spec type_(binary()) -> lustre@vdom@vattr:attribute(any()).
type_(Control_type) ->
    attribute(<<"type"/utf8>>, Control_type).

-file("src/lustre/attribute.gleam", 1152).
?DOC(
    " Specifies the value of an input or form control. Using this attribute will\n"
    " make sure the value is always in sync with your application's modelled, a\n"
    " practice known as [_controlled inputs_](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
    "\n"
    " If you'd like to let the DOM manage the value of an input but still set a\n"
    " default value for users to see, use the [`default_value`](#default_value)\n"
    " attribute instead.\n"
).
-spec value(binary()) -> lustre@vdom@vattr:attribute(any()).
value(Control_value) ->
    attribute(<<"value"/utf8>>, Control_value).

-file("src/lustre/attribute.gleam", 1165).
?DOC(
    " Set the default value of an input or form control. This is the value that will\n"
    " be shown to users when the input is first rendered and included in the form\n"
    " submission if the user does not change it.\n"
    "\n"
    " Just setting a default value and letting the DOM manage the state of an input\n"
    " is known as using [_uncontrolled inputs](https://github.com/lustre-labs/lustre/blob/main/pages/hints/controlled-vs-uncontrolled-inputs.md).\n"
    " Doing this means your application cannot set the value of an input after it\n"
    " is modified without using an effect.\n"
).
-spec default_value(binary()) -> lustre@vdom@vattr:attribute(any()).
default_value(Control_value) ->
    attribute(<<"virtual:defaultValue"/utf8>>, Control_value).

-file("src/lustre/attribute.gleam", 1174).
?DOC(
    " Sets a pragma directive for a document. This is used in meta tags to define\n"
    " behaviors the user agent should follow.\n"
).
-spec http_equiv(binary()) -> lustre@vdom@vattr:attribute(any()).
http_equiv(Value) ->
    attribute(<<"http-equiv"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1181).
?DOC(
    " Specifies the value of the meta element, which varies depending on the value\n"
    " of the name or http-equiv attribute.\n"
).
-spec content(binary()) -> lustre@vdom@vattr:attribute(any()).
content(Value) ->
    attribute(<<"content"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1188).
?DOC(
    " Declares the character encoding used in the document. When used with a meta\n"
    " element, this replaces the need for the `http_equiv(\"content-type\")` attribute.\n"
).
-spec charset(binary()) -> lustre@vdom@vattr:attribute(any()).
charset(Value) ->
    attribute(<<"charset"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1195).
?DOC(
    " Specifies the media types the resource applies to. This is commonly used with\n"
    " link elements for stylesheets to determine when they should be loaded.\n"
).
-spec media(binary()) -> lustre@vdom@vattr:attribute(any()).
media(Query) ->
    attribute(<<"media"/utf8>>, Query).

-file("src/lustre/attribute.gleam", 1209).
?DOC(
    " Indicates that the media resource should automatically begin playing as soon\n"
    " as it can do so without stopping. When not present, the media will not\n"
    " automatically play until the user initiates playback.\n"
    "\n"
    " > **Note**: Lustre's runtime augments this attribute. Whenever it is toggled\n"
    " > to true, the media will begin playing as if the element's `play()` method\n"
    " > was called.\n"
).
-spec autoplay(boolean()) -> lustre@vdom@vattr:attribute(any()).
autoplay(Auto_play) ->
    boolean_attribute(<<"autoplay"/utf8>>, Auto_play).

-file("src/lustre/attribute.gleam", 1216).
?DOC(
    " When present, this attribute shows the browser's built-in control panel for the\n"
    " media player, giving users control over playback, volume, seeking, and more.\n"
).
-spec controls(boolean()) -> lustre@vdom@vattr:attribute(any()).
controls(Show_controls) ->
    boolean_attribute(<<"controls"/utf8>>, Show_controls).

-file("src/lustre/attribute.gleam", 1223).
?DOC(
    " When present, this attribute indicates that the media should start over again\n"
    " from the beginning when it reaches the end.\n"
).
-spec loop(boolean()) -> lustre@vdom@vattr:attribute(any()).
loop(Should_loop) ->
    boolean_attribute(<<"loop"/utf8>>, Should_loop).

-file("src/lustre/attribute.gleam", 1230).
?DOC(
    " When present, this attribute indicates that the audio output of the media element\n"
    " should be initially silenced.\n"
).
-spec muted(boolean()) -> lustre@vdom@vattr:attribute(any()).
muted(Is_muted) ->
    boolean_attribute(<<"muted"/utf8>>, Is_muted).

-file("src/lustre/attribute.gleam", 1241).
?DOC(
    " Encourages the user agent to display video content within the element's\n"
    " playback area rather than in a separate window or fullscreen, especially on\n"
    " mobile devices.\n"
    "\n"
    " This attribute only acts as a *hint* to the user agent, and setting this to\n"
    " false does not imply that the video will be played in fullscreen.\n"
).
-spec playsinline(boolean()) -> lustre@vdom@vattr:attribute(any()).
playsinline(Play_inline) ->
    boolean_attribute(<<"playsinline"/utf8>>, Play_inline).

-file("src/lustre/attribute.gleam", 1248).
?DOC(
    " Specifies an image to be shown while the video is downloading, or until the\n"
    " user hits the play button.\n"
).
-spec poster(binary()) -> lustre@vdom@vattr:attribute(any()).
poster(Url) ->
    attribute(<<"poster"/utf8>>, Url).

-file("src/lustre/attribute.gleam", 1261).
?DOC(
    " Provides a hint to the browser about what the author thinks will lead to the\n"
    " best user experience. The following values are accepted:\n"
    "\n"
    " | Value      | Description                                                      |\n"
    " |------------|------------------------------------------------------------------|\n"
    " | \"auto\"     | Let's the user agent determine the best option                   |\n"
    " | \"metadata\" | Hints to the user agent that it can fetch the metadata only.     |\n"
    " | \"none\"     | Hints to the user agent that server traffic should be minimised. |\n"
).
-spec preload(binary()) -> lustre@vdom@vattr:attribute(any()).
preload(Value) ->
    attribute(<<"preload"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1280).
?DOC(
    " Specifies the mode for creating a shadow root on a template. Valid values\n"
    " include:\n"
    "\n"
    " | Value     | Description                                 |\n"
    " |-----------|---------------------------------------------|\n"
    " | \"open\"    | Shadow root's contents are accessible       |\n"
    " | \"closed\"  | Shadow root's contents are not accessible   |\n"
    "\n"
    " > **Note**: if you are pre-rendering a Lustre component you must make sure this\n"
    " > attribute matches the [`open_shadow_root`](./component.html#open_shadow_root)\n"
    " > configuration - or `\"closed\"` if not explicitly set - to ensure the shadow\n"
    " > root is created correctly.\n"
).
-spec shadowrootmode(binary()) -> lustre@vdom@vattr:attribute(any()).
shadowrootmode(Mode) ->
    attribute(<<"shadowrootmode"/utf8>>, Mode).

-file("src/lustre/attribute.gleam", 1287).
?DOC(
    " Indicates whether focus should be delegated to the shadow root when an element\n"
    " in the shadow tree gains focus.\n"
).
-spec shadowrootdelegatesfocus(boolean()) -> lustre@vdom@vattr:attribute(any()).
shadowrootdelegatesfocus(Delegates) ->
    boolean_attribute(<<"shadowrootdelegatesfocus"/utf8>>, Delegates).

-file("src/lustre/attribute.gleam", 1294).
?DOC(
    " Determines whether the shadow root can be cloned when the host element is\n"
    " cloned.\n"
).
-spec shadowrootclonable(boolean()) -> lustre@vdom@vattr:attribute(any()).
shadowrootclonable(Clonable) ->
    boolean_attribute(<<"shadowrootclonable"/utf8>>, Clonable).

-file("src/lustre/attribute.gleam", 1301).
?DOC(
    " Controls whether the shadow root should be preserved during serialization\n"
    " operations like copying to the clipboard or saving a page.\n"
).
-spec shadowrootserializable(boolean()) -> lustre@vdom@vattr:attribute(any()).
shadowrootserializable(Serializable) ->
    boolean_attribute(<<"shadowrootserializable"/utf8>>, Serializable).

-file("src/lustre/attribute.gleam", 1312).
?DOC(
    " A short, abbreviated description of the header cell's content provided as an\n"
    " alternative label to use for the header cell when referencing the cell in other\n"
    " contexts. Some user-agents, such as speech readers, may present this description\n"
    " before the content itself.\n"
).
-spec abbr(binary()) -> lustre@vdom@vattr:attribute(any()).
abbr(Value) ->
    attribute(<<"abbr"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1320).
?DOC(
    " A non-negative integer value indicating how many columns the header cell spans\n"
    " or extends. The default value is `1`. User agents dismiss values higher than\n"
    " `1000` as incorrect, defaulting such values to `1`.\n"
).
-spec colspan(integer()) -> lustre@vdom@vattr:attribute(any()).
colspan(Value) ->
    attribute(<<"colspan"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1327).
?DOC(
    " A list of space-separated strings corresponding to the id attributes of the\n"
    " `<th>` elements that provide the headers for this header cell.\n"
).
-spec headers(list(binary())) -> lustre@vdom@vattr:attribute(any()).
headers(Ids) ->
    attribute(<<"headers"/utf8>>, gleam@string:join(Ids, <<" "/utf8>>)).

-file("src/lustre/attribute.gleam", 1336).
?DOC(
    " A non-negative integer value indicating how many rows the header cell spans\n"
    " or extends. The default value is `1`; if its value is set to `0`, the header\n"
    " cell will extends to the end of the table grouping section, that the `<th>`\n"
    " belongs to. Values higher than `65534` are clipped at `65534`.\n"
).
-spec rowspan(integer()) -> lustre@vdom@vattr:attribute(any()).
rowspan(Value) ->
    attribute(
        <<"rowspan"/utf8>>,
        begin
            _pipe = Value,
            _pipe@1 = gleam@int:max(_pipe, 0),
            _pipe@2 = gleam@int:min(_pipe@1, 65534),
            erlang:integer_to_binary(_pipe@2)
        end
    ).

-file("src/lustre/attribute.gleam", 1348).
?DOC(
    " Specifies the number of consecutive columns a `<colgroup>` element spans. The\n"
    " value must be a positive integer greater than zero.\n"
).
-spec span(integer()) -> lustre@vdom@vattr:attribute(any()).
span(Value) ->
    attribute(<<"span"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1357).
?DOC(
    " The `scope` attribute specifies whether a header cell is a header for a row,\n"
    " column, or group of rows or columns. The following values are accepted:\n"
    "\n"
    " The `scope` attribute is only valid on `<th>` elements.\n"
).
-spec scope(binary()) -> lustre@vdom@vattr:attribute(any()).
scope(Value) ->
    attribute(<<"scope"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1366).
?DOC(
    " Add an `aria-*` attribute to an HTML element. The key will be prefixed by\n"
    " `aria-`.\n"
).
-spec aria(binary(), binary()) -> lustre@vdom@vattr:attribute(any()).
aria(Name, Value) ->
    attribute(<<"aria-"/utf8, Name/binary>>, Value).

-file("src/lustre/attribute.gleam", 1372).
?DOC("\n").
-spec role(binary()) -> lustre@vdom@vattr:attribute(any()).
role(Name) ->
    attribute(<<"role"/utf8>>, Name).

-file("src/lustre/attribute.gleam", 1379).
?DOC(
    " The aria-activedescendant attribute identifies the currently active element\n"
    " when focus is on a composite widget, combobox, textbox, group, or application.\n"
).
-spec aria_activedescendant(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_activedescendant(Id) ->
    aria(<<"activedescendant"/utf8>>, Id).

-file("src/lustre/attribute.gleam", 1388).
?DOC(
    " In ARIA live regions, the global aria-atomic attribute indicates whether\n"
    " assistive technologies such as a screen reader will present all, or only parts\n"
    " of, the changed region based on the change notifications defined by the\n"
    " aria-relevant attribute.\n"
).
-spec aria_atomic(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_atomic(Value) ->
    aria(<<"atomic"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1400).
?DOC(
    " The aria-autocomplete attribute indicates whether inputting text could trigger\n"
    " display of one or more predictions of the user's intended value for a combobox,\n"
    " searchbox, or textbox and specifies how predictions will be presented if they\n"
    " are made.\n"
).
-spec aria_autocomplete(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_autocomplete(Value) ->
    aria(<<"autocomplete"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1407).
?DOC(
    " The global aria-braillelabel property defines a string value that labels the\n"
    " current element, which is intended to be converted into Braille.\n"
).
-spec aria_braillelabel(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_braillelabel(Value) ->
    aria(<<"braillelabel"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1415).
?DOC(
    " The global aria-brailleroledescription attribute defines a human-readable,\n"
    " author-localized abbreviated description for the role of an element intended\n"
    " to be converted into Braille.\n"
).
-spec aria_brailleroledescription(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_brailleroledescription(Value) ->
    aria(<<"brailleroledescription"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1423).
?DOC(
    " Used in ARIA live regions, the global aria-busy state indicates an element is\n"
    " being modified and that assistive technologies may want to wait until the\n"
    " changes are complete before informing the user about the update.\n"
).
-spec aria_busy(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_busy(Value) ->
    aria(<<"busy"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1433).
?DOC(
    " The aria-checked attribute indicates the current \"checked\" state of checkboxes,\n"
    " radio buttons, and other widgets.\n"
).
-spec aria_checked(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_checked(Value) ->
    aria(<<"checked"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1440).
?DOC(
    " The aria-colcount attribute defines the total number of columns in a table,\n"
    " grid, or treegrid when not all columns are present in the DOM.\n"
).
-spec aria_colcount(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_colcount(Value) ->
    aria(<<"colcount"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1447).
?DOC(
    " The aria-colindex attribute defines an element's column index or position with\n"
    " respect to the total number of columns within a table, grid, or treegrid.\n"
).
-spec aria_colindex(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_colindex(Value) ->
    aria(<<"colindex"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1454).
?DOC(
    " The aria-colindextext attribute defines a human-readable text alternative of\n"
    " the numeric aria-colindex.\n"
).
-spec aria_colindextext(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_colindextext(Value) ->
    aria(<<"colindextext"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1461).
?DOC(
    " The aria-colspan attribute defines the number of columns spanned by a cell\n"
    " or gridcell within a table, grid, or treegrid.\n"
).
-spec aria_colspan(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_colspan(Value) ->
    aria(<<"colspan"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1469).
?DOC(
    " The global aria-controls property identifies the element (or elements) whose\n"
    " contents or presence are controlled by the element on which this attribute is\n"
    " set.\n"
).
-spec aria_controls(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_controls(Value) ->
    aria(<<"controls"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1476).
?DOC(
    " A non-null aria-current state on an element indicates that this element represents\n"
    " the current item within a container or set of related elements.\n"
).
-spec aria_current(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_current(Value) ->
    aria(<<"current"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1483).
?DOC(
    " The global aria-describedby attribute identifies the element (or elements)\n"
    " that describes the element on which the attribute is set.\n"
).
-spec aria_describedby(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_describedby(Value) ->
    aria(<<"describedby"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1490).
?DOC(
    " The global aria-description attribute defines a string value that describes\n"
    " or annotates the current element.\n"
).
-spec aria_description(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_description(Value) ->
    aria(<<"description"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1497).
?DOC(
    " The global aria-details attribute identifies the element (or elements) that\n"
    " provide additional information related to the object.\n"
).
-spec aria_details(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_details(Value) ->
    aria(<<"details"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1504).
?DOC(
    " The aria-disabled state indicates that the element is perceivable but disabled,\n"
    " so it is not editable or otherwise operable.\n"
).
-spec aria_disabled(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_disabled(Value) ->
    aria(<<"disabled"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1514).
?DOC(
    " The aria-errormessage attribute on an object identifies the element that\n"
    " provides an error message for that object.\n"
).
-spec aria_errormessage(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_errormessage(Value) ->
    aria(<<"errormessage"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1522).
?DOC(
    " The aria-expanded attribute is set on an element to indicate if a control is\n"
    " expanded or collapsed, and whether or not the controlled elements are displayed\n"
    " or hidden.\n"
).
-spec aria_expanded(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_expanded(Value) ->
    aria(<<"expanded"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1534).
?DOC(
    " The global aria-flowto attribute identifies the next element (or elements) in\n"
    " an alternate reading order of content. This allows assistive technology to\n"
    " override the general default of reading in document source order at the user's\n"
    " discretion.\n"
).
-spec aria_flowto(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_flowto(Value) ->
    aria(<<"flowto"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1542).
?DOC(
    " The aria-haspopup attribute indicates the availability and type of interactive\n"
    " popup element that can be triggered by the element on which the attribute is\n"
    " set.\n"
).
-spec aria_haspopup(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_haspopup(Value) ->
    aria(<<"haspopup"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1549).
?DOC(
    " The aria-hidden state indicates whether the element is exposed to an accessibility\n"
    " API.\n"
).
-spec aria_hidden(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_hidden(Value) ->
    aria(<<"hidden"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1559).
?DOC(
    " The aria-invalid state indicates the entered value does not conform to the\n"
    " format expected by the application.\n"
).
-spec aria_invalid(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_invalid(Value) ->
    aria(<<"invalid"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1566).
?DOC(
    " The global aria-keyshortcuts attribute indicates keyboard shortcuts that an\n"
    " author has implemented to activate or give focus to an element.\n"
).
-spec aria_keyshortcuts(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_keyshortcuts(Value) ->
    aria(<<"keyshortcuts"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1573).
?DOC(
    " The aria-label attribute defines a string value that can be used to name an\n"
    " element, as long as the element's role does not prohibit naming.\n"
).
-spec aria_label(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_label(Value) ->
    aria(<<"label"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1580).
?DOC(
    " The aria-labelledby attribute identifies the element (or elements) that labels\n"
    " the element it is applied to.\n"
).
-spec aria_labelledby(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_labelledby(Value) ->
    aria(<<"labelledby"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1587).
?DOC(
    " The aria-level attribute defines the hierarchical level of an element within\n"
    " a structure.\n"
).
-spec aria_level(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_level(Value) ->
    aria(<<"level"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1595).
?DOC(
    " The global aria-live attribute indicates that an element will be updated, and\n"
    " describes the types of updates the user agents, assistive technologies, and\n"
    " user can expect from the live region.\n"
).
-spec aria_live(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_live(Value) ->
    aria(<<"live"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1601).
?DOC(" The aria-modal attribute indicates whether an element is modal when displayed.\n").
-spec aria_modal(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_modal(Value) ->
    aria(<<"modal"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1611).
?DOC(
    " The aria-multiline attribute indicates whether a textbox accepts multiple\n"
    " lines of input or only a single line.\n"
).
-spec aria_multiline(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_multiline(Value) ->
    aria(<<"multiline"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1621).
?DOC(
    " The aria-multiselectable attribute indicates that the user may select more\n"
    " than one item from the current selectable descendants.\n"
).
-spec aria_multiselectable(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_multiselectable(Value) ->
    aria(<<"multiselectable"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1631).
?DOC(
    " The aria-orientation attribute indicates whether the element's orientation is\n"
    " horizontal, vertical, or unknown/ambiguous.\n"
).
-spec aria_orientation(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_orientation(Value) ->
    aria(<<"orientation"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1639).
?DOC(
    " The aria-owns attribute identifies an element (or elements) in order to define\n"
    " a visual, functional, or contextual relationship between a parent and its\n"
    " child elements when the DOM hierarchy cannot be used to represent the relationship.\n"
).
-spec aria_owns(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_owns(Value) ->
    aria(<<"owns"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1647).
?DOC(
    " The aria-placeholder attribute defines a short hint (a word or short phrase)\n"
    " intended to help the user with data entry when a form control has no value.\n"
    " The hint can be a sample value or a brief description of the expected format.\n"
).
-spec aria_placeholder(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_placeholder(Value) ->
    aria(<<"placeholder"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1655).
?DOC(
    " The aria-posinset attribute defines an element's number or position in the\n"
    " current set of listitems or treeitems when not all items are present in the\n"
    " DOM.\n"
).
-spec aria_posinset(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_posinset(Value) ->
    aria(<<"posinset"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1662).
?DOC(
    " The aria-pressed attribute indicates the current \"pressed\" state of a toggle\n"
    " button.\n"
).
-spec aria_pressed(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_pressed(Value) ->
    aria(<<"pressed"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1669).
?DOC(
    " The aria-readonly attribute indicates that the element is not editable, but is\n"
    " otherwise operable.\n"
).
-spec aria_readonly(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_readonly(Value) ->
    aria(<<"readonly"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1680).
?DOC(
    " Used in ARIA live regions, the global aria-relevant attribute indicates what\n"
    " notifications the user agent will trigger when the accessibility tree within\n"
    " a live region is modified.\n"
).
-spec aria_relevant(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_relevant(Value) ->
    aria(<<"relevant"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1687).
?DOC(
    " The aria-required attribute indicates that user input is required on the element\n"
    " before a form may be submitted.\n"
).
-spec aria_required(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_required(Value) ->
    aria(<<"required"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1697).
?DOC(
    " The aria-roledescription attribute defines a human-readable, author-localised\n"
    " description for the role of an element.\n"
).
-spec aria_roledescription(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_roledescription(Value) ->
    aria(<<"roledescription"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1704).
?DOC(
    " The aria-rowcount attribute defines the total number of rows in a table,\n"
    " grid, or treegrid.\n"
).
-spec aria_rowcount(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_rowcount(Value) ->
    aria(<<"rowcount"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1711).
?DOC(
    " The aria-rowindex attribute defines an element's position with respect to the\n"
    " total number of rows within a table, grid, or treegrid.\n"
).
-spec aria_rowindex(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_rowindex(Value) ->
    aria(<<"rowindex"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1718).
?DOC(
    " The aria-rowindextext attribute defines a human-readable text alternative of\n"
    " aria-rowindex.\n"
).
-spec aria_rowindextext(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_rowindextext(Value) ->
    aria(<<"rowindextext"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1725).
?DOC(
    " The aria-rowspan attribute defines the number of rows spanned by a cell or\n"
    " gridcell within a table, grid, or treegrid.\n"
).
-spec aria_rowspan(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_rowspan(Value) ->
    aria(<<"rowspan"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1732).
?DOC(
    " The aria-selected attribute indicates the current \"selected\" state of various\n"
    " widgets.\n"
).
-spec aria_selected(boolean()) -> lustre@vdom@vattr:attribute(any()).
aria_selected(Value) ->
    aria(<<"selected"/utf8>>, case Value of
            true ->
                <<"true"/utf8>>;

            false ->
                <<"false"/utf8>>
        end).

-file("src/lustre/attribute.gleam", 1742).
?DOC(
    " The aria-setsize attribute defines the number of items in the current set of\n"
    " listitems or treeitems when not all items in the set are present in the DOM.\n"
).
-spec aria_setsize(integer()) -> lustre@vdom@vattr:attribute(any()).
aria_setsize(Value) ->
    aria(<<"setsize"/utf8>>, erlang:integer_to_binary(Value)).

-file("src/lustre/attribute.gleam", 1749).
?DOC(
    " The aria-sort attribute indicates if items in a table or grid are sorted in\n"
    " ascending or descending order.\n"
).
-spec aria_sort(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_sort(Value) ->
    aria(<<"sort"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1756).
?DOC(
    " The aria-valuemax attribute defines the maximum allowed value for a range\n"
    " widget.\n"
).
-spec aria_valuemax(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_valuemax(Value) ->
    aria(<<"valuemax"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1763).
?DOC(
    " The aria-valuemin attribute defines the minimum allowed value for a range\n"
    " widget.\n"
).
-spec aria_valuemin(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_valuemin(Value) ->
    aria(<<"valuemin"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1769).
?DOC(" The aria-valuenow attribute defines the current value for a range widget.\n").
-spec aria_valuenow(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_valuenow(Value) ->
    aria(<<"valuenow"/utf8>>, Value).

-file("src/lustre/attribute.gleam", 1776).
?DOC(
    " The aria-valuetext attribute defines the human-readable text alternative of\n"
    " aria-valuenow for a range widget.\n"
).
-spec aria_valuetext(binary()) -> lustre@vdom@vattr:attribute(any()).
aria_valuetext(Value) ->
    aria(<<"valuetext"/utf8>>, Value).
