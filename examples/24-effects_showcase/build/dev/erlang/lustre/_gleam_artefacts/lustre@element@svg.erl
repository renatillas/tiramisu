-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/element/svg.gleam").
-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, view/2, desc/2, metadata/2, title/2, filter/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/element/svg.gleam", 24).
?DOC("\n").
-spec animate(list(lustre@vdom@vattr:attribute(XUA))) -> lustre@vdom@vnode:element(XUA).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 29).
?DOC("\n").
-spec animate_motion(list(lustre@vdom@vattr:attribute(XUE))) -> lustre@vdom@vnode:element(XUE).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 34).
?DOC("\n").
-spec animate_transform(list(lustre@vdom@vattr:attribute(XUI))) -> lustre@vdom@vnode:element(XUI).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 39).
?DOC("\n").
-spec mpath(list(lustre@vdom@vattr:attribute(XUM))) -> lustre@vdom@vnode:element(XUM).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 44).
?DOC("\n").
-spec set(list(lustre@vdom@vattr:attribute(XUQ))) -> lustre@vdom@vnode:element(XUQ).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 51).
?DOC("\n").
-spec circle(list(lustre@vdom@vattr:attribute(XUU))) -> lustre@vdom@vnode:element(XUU).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 56).
?DOC("\n").
-spec ellipse(list(lustre@vdom@vattr:attribute(XUY))) -> lustre@vdom@vnode:element(XUY).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 61).
?DOC("\n").
-spec line(list(lustre@vdom@vattr:attribute(XVC))) -> lustre@vdom@vnode:element(XVC).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 66).
?DOC("\n").
-spec polygon(list(lustre@vdom@vattr:attribute(XVG))) -> lustre@vdom@vnode:element(XVG).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 71).
?DOC("\n").
-spec polyline(list(lustre@vdom@vattr:attribute(XVK))) -> lustre@vdom@vnode:element(XVK).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 76).
?DOC("\n").
-spec rect(list(lustre@vdom@vattr:attribute(XVO))) -> lustre@vdom@vnode:element(XVO).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 83).
?DOC("\n").
-spec a(
    list(lustre@vdom@vattr:attribute(XVS)),
    list(lustre@vdom@vnode:element(XVS))
) -> lustre@vdom@vnode:element(XVS).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 91).
?DOC("\n").
-spec defs(
    list(lustre@vdom@vattr:attribute(XVY)),
    list(lustre@vdom@vnode:element(XVY))
) -> lustre@vdom@vnode:element(XVY).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 99).
?DOC("\n").
-spec g(
    list(lustre@vdom@vattr:attribute(XWE)),
    list(lustre@vdom@vnode:element(XWE))
) -> lustre@vdom@vnode:element(XWE).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 107).
?DOC("\n").
-spec marker(
    list(lustre@vdom@vattr:attribute(XWK)),
    list(lustre@vdom@vnode:element(XWK))
) -> lustre@vdom@vnode:element(XWK).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 115).
?DOC("\n").
-spec mask(
    list(lustre@vdom@vattr:attribute(XWQ)),
    list(lustre@vdom@vnode:element(XWQ))
) -> lustre@vdom@vnode:element(XWQ).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 123).
?DOC("\n").
-spec missing_glyph(
    list(lustre@vdom@vattr:attribute(XWW)),
    list(lustre@vdom@vnode:element(XWW))
) -> lustre@vdom@vnode:element(XWW).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 131).
?DOC("\n").
-spec pattern(
    list(lustre@vdom@vattr:attribute(XXC)),
    list(lustre@vdom@vnode:element(XXC))
) -> lustre@vdom@vnode:element(XXC).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 139).
?DOC("\n").
-spec svg(
    list(lustre@vdom@vattr:attribute(XXI)),
    list(lustre@vdom@vnode:element(XXI))
) -> lustre@vdom@vnode:element(XXI).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 147).
?DOC("\n").
-spec switch(
    list(lustre@vdom@vattr:attribute(XXO)),
    list(lustre@vdom@vnode:element(XXO))
) -> lustre@vdom@vnode:element(XXO).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 155).
?DOC("\n").
-spec symbol(
    list(lustre@vdom@vattr:attribute(XXU)),
    list(lustre@vdom@vnode:element(XXU))
) -> lustre@vdom@vnode:element(XXU).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 163).
?DOC("\n").
-spec view(
    list(lustre@vdom@vattr:attribute(XYA)),
    list(lustre@vdom@vnode:element(XYA))
) -> lustre@vdom@vnode:element(XYA).
view(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"view"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 173).
?DOC("\n").
-spec desc(
    list(lustre@vdom@vattr:attribute(XYG)),
    list(lustre@vdom@vnode:element(XYG))
) -> lustre@vdom@vnode:element(XYG).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 181).
?DOC("\n").
-spec metadata(
    list(lustre@vdom@vattr:attribute(XYM)),
    list(lustre@vdom@vnode:element(XYM))
) -> lustre@vdom@vnode:element(XYM).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 189).
?DOC("\n").
-spec title(
    list(lustre@vdom@vattr:attribute(XYS)),
    list(lustre@vdom@vnode:element(XYS))
) -> lustre@vdom@vnode:element(XYS).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 199).
?DOC("\n").
-spec filter(
    list(lustre@vdom@vattr:attribute(XYY)),
    list(lustre@vdom@vnode:element(XYY))
) -> lustre@vdom@vnode:element(XYY).
filter(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"filter"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 207).
?DOC("\n").
-spec fe_blend(list(lustre@vdom@vattr:attribute(XZE))) -> lustre@vdom@vnode:element(XZE).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 212).
?DOC("\n").
-spec fe_color_matrix(list(lustre@vdom@vattr:attribute(XZI))) -> lustre@vdom@vnode:element(XZI).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 217).
?DOC("\n").
-spec fe_component_transfer(list(lustre@vdom@vattr:attribute(XZM))) -> lustre@vdom@vnode:element(XZM).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 222).
?DOC("\n").
-spec fe_composite(list(lustre@vdom@vattr:attribute(XZQ))) -> lustre@vdom@vnode:element(XZQ).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 227).
?DOC("\n").
-spec fe_convolve_matrix(list(lustre@vdom@vattr:attribute(XZU))) -> lustre@vdom@vnode:element(XZU).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 232).
?DOC("\n").
-spec fe_diffuse_lighting(
    list(lustre@vdom@vattr:attribute(XZY)),
    list(lustre@vdom@vnode:element(XZY))
) -> lustre@vdom@vnode:element(XZY).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 240).
?DOC("\n").
-spec fe_displacement_map(list(lustre@vdom@vattr:attribute(YAE))) -> lustre@vdom@vnode:element(YAE).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 245).
?DOC("\n").
-spec fe_drop_shadow(list(lustre@vdom@vattr:attribute(YAI))) -> lustre@vdom@vnode:element(YAI).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 250).
?DOC("\n").
-spec fe_flood(list(lustre@vdom@vattr:attribute(YAM))) -> lustre@vdom@vnode:element(YAM).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 255).
?DOC("\n").
-spec fe_func_a(list(lustre@vdom@vattr:attribute(YAQ))) -> lustre@vdom@vnode:element(YAQ).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 260).
?DOC("\n").
-spec fe_func_b(list(lustre@vdom@vattr:attribute(YAU))) -> lustre@vdom@vnode:element(YAU).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 265).
?DOC("\n").
-spec fe_func_g(list(lustre@vdom@vattr:attribute(YAY))) -> lustre@vdom@vnode:element(YAY).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 270).
?DOC("\n").
-spec fe_func_r(list(lustre@vdom@vattr:attribute(YBC))) -> lustre@vdom@vnode:element(YBC).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 275).
?DOC("\n").
-spec fe_gaussian_blur(list(lustre@vdom@vattr:attribute(YBG))) -> lustre@vdom@vnode:element(YBG).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 280).
?DOC("\n").
-spec fe_image(list(lustre@vdom@vattr:attribute(YBK))) -> lustre@vdom@vnode:element(YBK).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 285).
?DOC("\n").
-spec fe_merge(
    list(lustre@vdom@vattr:attribute(YBO)),
    list(lustre@vdom@vnode:element(YBO))
) -> lustre@vdom@vnode:element(YBO).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 293).
?DOC("\n").
-spec fe_merge_node(list(lustre@vdom@vattr:attribute(YBU))) -> lustre@vdom@vnode:element(YBU).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 298).
?DOC("\n").
-spec fe_morphology(list(lustre@vdom@vattr:attribute(YBY))) -> lustre@vdom@vnode:element(YBY).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 303).
?DOC("\n").
-spec fe_offset(list(lustre@vdom@vattr:attribute(YCC))) -> lustre@vdom@vnode:element(YCC).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 308).
?DOC("\n").
-spec fe_specular_lighting(
    list(lustre@vdom@vattr:attribute(YCG)),
    list(lustre@vdom@vnode:element(YCG))
) -> lustre@vdom@vnode:element(YCG).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 316).
?DOC("\n").
-spec fe_tile(
    list(lustre@vdom@vattr:attribute(YCM)),
    list(lustre@vdom@vnode:element(YCM))
) -> lustre@vdom@vnode:element(YCM).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 324).
?DOC("\n").
-spec fe_turbulence(list(lustre@vdom@vattr:attribute(YCS))) -> lustre@vdom@vnode:element(YCS).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 331).
?DOC("\n").
-spec linear_gradient(
    list(lustre@vdom@vattr:attribute(YCW)),
    list(lustre@vdom@vnode:element(YCW))
) -> lustre@vdom@vnode:element(YCW).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 339).
?DOC("\n").
-spec radial_gradient(
    list(lustre@vdom@vattr:attribute(YDC)),
    list(lustre@vdom@vnode:element(YDC))
) -> lustre@vdom@vnode:element(YDC).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 347).
?DOC("\n").
-spec stop(list(lustre@vdom@vattr:attribute(YDI))) -> lustre@vdom@vnode:element(YDI).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 354).
?DOC("\n").
-spec image(list(lustre@vdom@vattr:attribute(YDM))) -> lustre@vdom@vnode:element(YDM).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 359).
?DOC("\n").
-spec path(list(lustre@vdom@vattr:attribute(YDQ))) -> lustre@vdom@vnode:element(YDQ).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 364).
?DOC("\n").
-spec text(list(lustre@vdom@vattr:attribute(YDU)), binary()) -> lustre@vdom@vnode:element(YDU).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("src/lustre/element/svg.gleam", 369).
?DOC("\n").
-spec use_(list(lustre@vdom@vattr:attribute(YDY))) -> lustre@vdom@vnode:element(YDY).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 376).
?DOC("\n").
-spec fe_distant_light(list(lustre@vdom@vattr:attribute(YEC))) -> lustre@vdom@vnode:element(YEC).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 381).
?DOC("\n").
-spec fe_point_light(list(lustre@vdom@vattr:attribute(YEG))) -> lustre@vdom@vnode:element(YEG).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 386).
?DOC("\n").
-spec fe_spot_light(list(lustre@vdom@vattr:attribute(YEK))) -> lustre@vdom@vnode:element(YEK).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 393).
?DOC("\n").
-spec clip_path(
    list(lustre@vdom@vattr:attribute(YEO)),
    list(lustre@vdom@vnode:element(YEO))
) -> lustre@vdom@vnode:element(YEO).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 401).
?DOC("\n").
-spec script(list(lustre@vdom@vattr:attribute(YEU)), binary()) -> lustre@vdom@vnode:element(YEU).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-file("src/lustre/element/svg.gleam", 406).
?DOC("\n").
-spec style(list(lustre@vdom@vattr:attribute(YEY)), binary()) -> lustre@vdom@vnode:element(YEY).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-file("src/lustre/element/svg.gleam", 413).
?DOC("\n").
-spec foreign_object(
    list(lustre@vdom@vattr:attribute(YFC)),
    list(lustre@vdom@vnode:element(YFC))
) -> lustre@vdom@vnode:element(YFC).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 421).
?DOC("\n").
-spec text_path(
    list(lustre@vdom@vattr:attribute(YFI)),
    list(lustre@vdom@vnode:element(YFI))
) -> lustre@vdom@vnode:element(YFI).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 429).
?DOC("\n").
-spec tspan(
    list(lustre@vdom@vattr:attribute(YFO)),
    list(lustre@vdom@vnode:element(YFO))
) -> lustre@vdom@vnode:element(YFO).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
