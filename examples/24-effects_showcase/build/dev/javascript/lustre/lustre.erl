-module(lustre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre.gleam").
-export([component/4, application/3, element/1, simple/3, register/2, send/2, dispatch/1, shutdown/0, is_browser/0, start/3, is_registered/1, start_server_component/2]).
-export_type([app/3, error/0, runtime/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Lustre is a framework for rendering Web applications and components using\n"
    " Gleam. This module contains the core API for constructing and communicating\n"
    " with Lustre applications. If you're new to Lustre or frontend development in\n"
    " general, make sure you check out the [examples](https://github.com/lustre-labs/lustre/tree/main/examples)\n"
    " or the [quickstart guide](./guide/01-quickstart.html) to get up to speed!\n"
    "\n"
    " Lustre currently has three kinds of application:\n"
    "\n"
    " 1. A client-side single-page application: think Elm or React or Vue. These\n"
    "    are applications that run in the client's browser and are responsible for\n"
    "    rendering the entire page.\n"
    "\n"
    " 2. A client-side component: an encapsulated Lustre application that can be\n"
    "    rendered inside another Lustre application as a Web Component. Communication\n"
    "    happens via attributes and event listeners, like any other HTML element.\n"
    "\n"
    " 3. A server component. These are applications that run anywhere Gleam runs\n"
    "    and communicate with any number of connected clients by sending them\n"
    "    patches to apply to their DOM.\n"
    "\n"
    "    There are two pieces to a server component: the main server component\n"
    "    runtime that contains your application logic, and a client-side runtime\n"
    "    that listens for patches over a WebSocket and applies them to the DOM.\n"
    "\n"
    "    The server component runtime can run anywhere Gleam does, but the\n"
    "    client-side runtime must be run in a browser. To use it, either render the\n"
    "    [provided script element](./lustre/server_component.html#script) or serve\n"
    "    the pre-bundled scripts found in Lustre's `priv/` directory directly.\n"
    "\n"
    " No matter where a Lustre application runs, it will always follow the same\n"
    " Model-View-Update architecture. Popularised by Elm (where it is known as The\n"
    " Elm Architecture), this pattern has since made its way into many other\n"
    " languages and frameworks and has proven to be a robust and reliable way to\n"
    " build complex user interfaces.\n"
    "\n"
    " There are three main building blocks to the Model-View-Update architecture:\n"
    "\n"
    " - A `Model` that represents your application's state and an `init` function\n"
    "   to create it.\n"
    "\n"
    " - A `Msg` type that represents all the different ways the outside world can\n"
    "   communicate with your application and an `update` function that modifies\n"
    "   your model in response to those messages.\n"
    "\n"
    " - A `view` function that renders your model to HTML, represented as an\n"
    "   `Element`.\n"
    "\n"
    " To see how those pieces fit together, here's a little diagram:\n"
    "\n"
    " ```text\n"
    "                                          +--------+\n"
    "                                          |        |\n"
    "                                          | update |\n"
    "                                          |        |\n"
    "                                          +--------+\n"
    "                                            ^    |\n"
    "                                            |    |\n"
    "                                        Msg |    | #(Model, Effect(Msg))\n"
    "                                            |    |\n"
    "                                            |    v\n"
    " +------+                         +------------------------+\n"
    " |      |  #(Model, Effect(Msg))  |                        |\n"
    " | init |------------------------>|     Lustre Runtime     |\n"
    " |      |                         |                        |\n"
    " +------+                         +------------------------+\n"
    "                                            ^    |\n"
    "                                            |    |\n"
    "                                        Msg |    | Model\n"
    "                                            |    |\n"
    "                                            |    v\n"
    "                                          +--------+\n"
    "                                          |        |\n"
    "                                          |  view  |\n"
    "                                          |        |\n"
    "                                          +--------+\n"
    " ```\n"
    "\n"
    " The `Effect` type here encompasses things like HTTP requests and other kinds\n"
    " of communication with the \"outside world\". You can read more about effects\n"
    " and their purpose in the [`effect`](./effect.html) module.\n"
    "\n"
    " For many kinds of apps, you can take these three building blocks and put\n"
    " together a Lustre application capable of running *anywhere*. Because of that,\n"
    " we like to describe Lustre as a **universal framework**.\n"
    "\n"
    " ## Guides\n"
    "\n"
    " A number of guides have been written to teach you how to use Lustre to build\n"
    " different kinds of applications. If you're just getting started with Lustre\n"
    " or frontend development, we recommend reading through them in order:\n"
    "\n"
    " - [`01-quickstart`](./guide/01-quickstart.html)\n"
    " - [`02-state-management`](./guide/02-state-management.html)\n"
    " - [`03-side-effects`](./guide/03-side-effects.html)\n"
    " - [`04-spa-deployments`](./guide/04-spa-deployments.html)\n"
    " - [`05-server-side-rendering`](./guide/05-server-side-rendering.html)\n"
    " - [`06-full-stack-applications`](./guide/06-full-stack-applications.html)\n"
    " - [`07-full-stack-deployments`](./guide/07-full-stack-deployments.html)\n"
    " - [`08-components`](./guide/08-components.html)\n"
    " - [`09-server-components`](./guide/09-server-components.html)\n"
    "\n"
    " This list of guides is likely to grow over time, so be sure to check back\n"
    " every now and then to see what's new!\n"
    "\n"
    " ## Examples\n"
    "\n"
    " If you prefer to learn by seeing and adapting existing code, there are also\n"
    " a number of examples in the [Lustre GitHub repository](https://github.com/lustre-labs/lustre)\n"
    " that each demonstrate a different concept or idea. While we can't list them\n"
    " all here, some of the more important ones are:\n"
    "\n"
    " - [`Controlled inputs`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/01-controlled-inputs)\n"
    " - [`Handling forms`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/04-forms)\n"
    " - [`Making HTTP requests`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/01-http-requests)\n"
    " - [`Routing`](https://github.com/lustre-labs/lustre/tree/main/examples/04-applications/01-routing)\n"
    " - [`Creating components`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/01-basic-setup)\n"
    " - [`Creating server components`](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/01-basic-setup)\n"
    "\n"
    " ## Companion libraries\n"
    "\n"
    " While this package contains the runtime and API necessary for building and\n"
    " rendering applications, there is also a small collection of companion libraries\n"
    " built to make building Lustre applications easier:\n"
    "\n"
    " - [lustre/ui](https://github.com/lustre-labs/ui) is a collection of pre-designed\n"
    "   elements and design tokens for building user interfaces with Lustre.\n"
    "\n"
    " - [lustre/ssg](https://github.com/lustre-labs/ssg) is a simple static site\n"
    "   generator that you can use to produce static HTML documents from your Lustre\n"
    "   applications.\n"
    "\n"
    " Both of these packages are heavy works in progress: any feedback or contributions\n"
    " are very welcome!\n"
    "\n"
    " ## Getting help\n"
    "\n"
    " If you're having trouble with Lustre or not sure what the right way to do\n"
    " something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).\n"
    " You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).\n"
    "\n"
    " ## Contributing\n"
    "\n"
    " The best way to contribute to Lustre is by building things! If you've built\n"
    " something cool with Lustre you want to share then please share it on the\n"
    " `#sharing` channel in the  [Gleam Discord server](https://discord.gg/Fm8Pwmy).\n"
    " You can also tag Hayleigh on BlueSky [@hayleigh.dev](https://bsky.app/profile/hayleigh.dev).\n"
    "\n"
    " If you run into any issues or have ideas for how to improve Lustre, please\n"
    " open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).\n"
    " Fixes and improvements to the documentation are also very welcome!\n"
    "\n"
    " Finally, if you'd like, you can support the project through\n"
    " [GitHub Sponsors](https://github.com/sponsors/hayleigh-dot-dev). Sponsorship\n"
    " helps fund the copious amounts of coffee that goes into building and maintaining\n"
    " Lustre, and is very much appreciated!\n"
    "\n"
).

-opaque app(SNC, SND, SNE) :: {app,
        fun((SNC) -> {SND, lustre@effect:effect(SNE)}),
        fun((SND, SNE) -> {SND, lustre@effect:effect(SNE)}),
        fun((SND) -> lustre@vdom@vnode:element(SNE)),
        lustre@component:config(SNE)}.

-type error() :: {actor_error, gleam@otp@actor:start_error()} |
    {bad_component_name, binary()} |
    {component_already_registered, binary()} |
    {element_not_found, binary()} |
    not_a_browser.

-type runtime(SNF) :: any() | {gleam_phantom, SNF}.

-file("src/lustre.gleam", 307).
?DOC(
    " A `component` is a type of Lustre application designed to be embedded within\n"
    " another application and has its own encapsulated update loop. This constructor\n"
    " is almost identical to the [`application`](#application) constructor, but it\n"
    " also allows you to specify a dictionary of attribute names and decoders.\n"
    "\n"
    " When a component is rendered in a parent application, it can receive data from\n"
    " the parent application through HTML attributes and properties just like any\n"
    " other HTML element. This dictionary of decoders allows you to specify how to\n"
    " decode those attributes into messages your component's update loop can handle.\n"
    "\n"
    " > **Note**: Lustre components are conceptually a lot \"heavier\" than components\n"
    " > in frameworks like React. They should be used for more complex UI widgets\n"
    " > like a combobox with complex keyboard interactions rather than simple things\n"
    " > like buttons or text inputs. Where possible try to think about how to build\n"
    " > your UI with simple view functions (functions that return [Elements](./lustre/element.html#Element))\n"
    " > and only reach for components when you really need to encapsulate that update\n"
    " > loop.\n"
).
-spec component(
    fun((SOE) -> {SOF, lustre@effect:effect(SOG)}),
    fun((SOF, SOG) -> {SOF, lustre@effect:effect(SOG)}),
    fun((SOF) -> lustre@vdom@vnode:element(SOG)),
    list(lustre@component:option(SOG))
) -> app(SOE, SOF, SOG).
component(Init, Update, View, Options) ->
    {app, Init, Update, View, lustre@component:new(Options)}.

-file("src/lustre.gleam", 281).
?DOC(
    " A complete Lustre application that follows the Model-View-Update architecture\n"
    " and can handle side effects like HTTP requests or querying the DOM. Most real\n"
    " Lustre applications will use this constructor.\n"
    "\n"
    " To learn more about effects and their purpose, take a look at the\n"
    " [`effect`](./lustre/effect.html) module or the\n"
    " [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).\n"
).
-spec application(
    fun((SNV) -> {SNW, lustre@effect:effect(SNX)}),
    fun((SNW, SNX) -> {SNW, lustre@effect:effect(SNX)}),
    fun((SNW) -> lustre@vdom@vnode:element(SNX))
) -> app(SNV, SNW, SNX).
application(Init, Update, View) ->
    {app, Init, Update, View, lustre@component:new([])}.

-file("src/lustre.gleam", 246).
?DOC(
    " The simplest type of Lustre application. The `element` application is\n"
    " primarily used for demonstration purposes. It renders a static Lustre `Element`\n"
    " on the page and does not have any state or update logic.\n"
).
-spec element(lustre@vdom@vnode:element(SNI)) -> app(any(), nil, SNI).
element(View) ->
    application(
        fun(_) -> {nil, lustre@effect:none()} end,
        fun(_, _) -> {nil, lustre@effect:none()} end,
        fun(_) -> View end
    ).

-file("src/lustre.gleam", 262).
?DOC(
    " A `simple` application has the basic Model-View-Update building blocks present\n"
    " in all Lustre applications, but it cannot handle effects. This is a great way\n"
    " to learn the basics of Lustre and its architecture.\n"
    "\n"
    " Once you're comfortable with the Model-View-Update loop and want to start\n"
    " building more complex applications that can communicate with the outside world,\n"
    " you'll want to use the [`application`](#application) constructor instead.\n"
).
-spec simple(
    fun((SNO) -> SNP),
    fun((SNP, SNQ) -> SNP),
    fun((SNP) -> lustre@vdom@vnode:element(SNQ))
) -> app(SNO, SNP, SNQ).
simple(Init, Update, View) ->
    Init@1 = fun(Start_args) -> {Init(Start_args), lustre@effect:none()} end,
    Update@1 = fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
    application(Init@1, Update@1, View).

-file("src/lustre.gleam", 341).
-spec do_start(app(SOY, any(), SPA), binary(), SOY) -> {ok, runtime(SPA)} |
    {error, error()}.
do_start(_, _, _) ->
    {error, not_a_browser}.

-file("src/lustre.gleam", 396).
?DOC(
    " Register a Lustre application as a Web Component. This lets you render that\n"
    " application in another Lustre application's view or use it as a Custom Element\n"
    " outside of Lustre entirely.The provided application can only have `Nil` start_args\n"
    " because there is no way to provide an initial value for start_args when using a\n"
    " Custom Element!\n"
    "\n"
    " The second argument is the name of the Custom Element. This is the name you'd\n"
    " use in HTML to render the component. For example, if you register a component\n"
    " with the name `my-component`, you'd use it in HTML by writing `<my-component>`\n"
    " or in Lustre by rendering `element(\"my-component\", [], [])`.\n"
    "\n"
    " > **Note**: There are [some rules](https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define#valid_custom_element_names)\n"
    " > for what names are valid for a Custom Element. The most important one is that\n"
    " > the name *must* contain a hypen so that it can be distinguished from standard\n"
    " > HTML elements.\n"
    "\n"
    " > **Note**: This function is only meaningful when running in the browser and will\n"
    " > produce a `NotABrowser` error if called anywhere else. For server contexts,\n"
    " > you can render a Lustre server component using [`start_server_component`](#start_server_component)\n"
    " > or [`start_actor`](#start_actor) instead.\n"
).
-spec register(app(nil, any(), any()), binary()) -> {ok, nil} | {error, error()}.
register(_, _) ->
    {error, not_a_browser}.

-file("src/lustre.gleam", 408).
?DOC(
    " Send a message to a running application's runtime directly. This function is\n"
    " primarily used for sending decoded client messages to a server component's\n"
    " runtime.\n"
).
-spec send(runtime(SPX), lustre@runtime@server@runtime:message(SPX)) -> nil.
send(Runtime, Message) ->
    gleam@erlang@process:send(Runtime, Message).

-file("src/lustre.gleam", 417).
?DOC(
    " Dispatch a message to a running application's `update` function. This can be\n"
    " used as a way for the outside world to communicate with a Lustre app without\n"
    " the app needing to initiate things with an effect.\n"
).
-spec dispatch(SQA) -> lustre@runtime@server@runtime:message(SQA).
dispatch(Msg) ->
    {effect_dispatched_message, Msg}.

-file("src/lustre.gleam", 426).
?DOC(
    " Instruct a running application to shut down. For client SPAs this will stop\n"
    " the runtime and unmount the app from the DOM. For server components, this will\n"
    " stop the runtime and prevent any further patches from being sent to connected\n"
    " clients.\n"
).
-spec shutdown() -> lustre@runtime@server@runtime:message(any()).
shutdown() ->
    system_requested_shutdown.

-file("src/lustre.gleam", 441).
?DOC(
    " Gleam's conditional compilation makes it possible to have different implementations\n"
    " of a function for different targets, but it's not possible to know what runtime\n"
    " you're targeting at compile-time.\n"
    "\n"
    " This is problematic if you're using server components with a JavaScript\n"
    " backend because you'll want to know whether you're currently running on your\n"
    " server or in the browser: this function tells you that!\n"
).
-spec is_browser() -> boolean().
is_browser() ->
    false.

-file("src/lustre.gleam", 330).
?DOC(
    " Start a constructed application as a client-side single-page application (SPA).\n"
    " This is the most typical way to start a Lustre application and will *only* work\n"
    " in the browser\n"
    "\n"
    " The second argument is a [CSS selector](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector)\n"
    " used to locate the DOM element where the application will be mounted on to.\n"
    " The most common selectors are `\"#app\"` to target an element with an id of `app`\n"
    " or `[data-lustre-app]` to target an element with a `data-lustre-app` attribute.\n"
    "\n"
    " The third argument is the starting data for the application. This is passed\n"
    " to the application's `init` function.\n"
).
-spec start(app(SOP, any(), SOR), binary(), SOP) -> {ok, runtime(SOR)} |
    {error, error()}.
start(App, Selector, Start_args) ->
    gleam@bool:guard(
        not is_browser(),
        {error, not_a_browser},
        fun() -> do_start(App, Selector, Start_args) end
    ).

-file("src/lustre.gleam", 450).
?DOC(
    " Check if the given component name has already been registered as a Custom\n"
    " Element. This is particularly useful in contexts where _other web components_\n"
    " may have been registered and you must avoid collisions.\n"
).
-spec is_registered(binary()) -> boolean().
is_registered(_) ->
    false.

-file("src/lustre.gleam", 360).
?DOC(
    " Start an application as a server component. This runs in a headless mode and\n"
    " doesn't render anything to the DOM. Instead, multiple clients can be attached\n"
    " using the [`add_renderer`](#add_renderer) action.\n"
    "\n"
    " If a server component starts successfully, this function will return a callback\n"
    " that can be used to send actions to the component runtime.\n"
    "\n"
    " A server component will keep running until the program is terminated or the\n"
    " [`shutdown`](#shutdown) action is sent to it.\n"
).
-spec start_server_component(app(SPH, any(), SPJ), SPH) -> {ok, runtime(SPJ)} |
    {error, error()}.
start_server_component(App, Start_args) ->
    _pipe = (erlang:element(2, App))(Start_args),
    _pipe@1 = lustre@runtime@server@runtime:start(
        _pipe,
        erlang:element(3, App),
        erlang:element(4, App),
        lustre@component:to_server_component_config(erlang:element(5, App))
    ),
    _pipe@2 = gleam@result:map(_pipe@1, fun gleam@function:identity/1),
    gleam@result:map_error(_pipe@2, fun(Field@0) -> {actor_error, Field@0} end).
