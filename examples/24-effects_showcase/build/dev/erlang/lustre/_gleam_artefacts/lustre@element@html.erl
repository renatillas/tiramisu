-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/element/html.gleam").
-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/2, template/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/element/html.gleam", 11).
?DOC("\n").
-spec html(
    list(lustre@vdom@vattr:attribute(SXH)),
    list(lustre@vdom@vnode:element(SXH))
) -> lustre@vdom@vnode:element(SXH).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 18).
-spec text(binary()) -> lustre@vdom@vnode:element(any()).
text(Content) ->
    lustre@element:text(Content).

-file("src/lustre/element/html.gleam", 25).
?DOC("\n").
-spec base(list(lustre@vdom@vattr:attribute(SXP))) -> lustre@vdom@vnode:element(SXP).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 30).
?DOC("\n").
-spec head(
    list(lustre@vdom@vattr:attribute(SXT)),
    list(lustre@vdom@vnode:element(SXT))
) -> lustre@vdom@vnode:element(SXT).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 38).
?DOC("\n").
-spec link(list(lustre@vdom@vattr:attribute(SXZ))) -> lustre@vdom@vnode:element(SXZ).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 43).
?DOC("\n").
-spec meta(list(lustre@vdom@vattr:attribute(SYD))) -> lustre@vdom@vnode:element(SYD).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 48).
?DOC("\n").
-spec style(list(lustre@vdom@vattr:attribute(SYH)), binary()) -> lustre@vdom@vnode:element(SYH).
style(Attrs, Css) ->
    lustre@element:unsafe_raw_html(<<""/utf8>>, <<"style"/utf8>>, Attrs, Css).

-file("src/lustre/element/html.gleam", 53).
?DOC("\n").
-spec title(list(lustre@vdom@vattr:attribute(SYL)), binary()) -> lustre@vdom@vnode:element(SYL).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-file("src/lustre/element/html.gleam", 60).
?DOC("\n").
-spec body(
    list(lustre@vdom@vattr:attribute(SYP)),
    list(lustre@vdom@vnode:element(SYP))
) -> lustre@vdom@vnode:element(SYP).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 70).
?DOC("\n").
-spec address(
    list(lustre@vdom@vattr:attribute(SYV)),
    list(lustre@vdom@vnode:element(SYV))
) -> lustre@vdom@vnode:element(SYV).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 78).
?DOC("\n").
-spec article(
    list(lustre@vdom@vattr:attribute(SZB)),
    list(lustre@vdom@vnode:element(SZB))
) -> lustre@vdom@vnode:element(SZB).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 86).
?DOC("\n").
-spec aside(
    list(lustre@vdom@vattr:attribute(SZH)),
    list(lustre@vdom@vnode:element(SZH))
) -> lustre@vdom@vnode:element(SZH).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 94).
?DOC("\n").
-spec footer(
    list(lustre@vdom@vattr:attribute(SZN)),
    list(lustre@vdom@vnode:element(SZN))
) -> lustre@vdom@vnode:element(SZN).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 102).
?DOC("\n").
-spec header(
    list(lustre@vdom@vattr:attribute(SZT)),
    list(lustre@vdom@vnode:element(SZT))
) -> lustre@vdom@vnode:element(SZT).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 110).
?DOC("\n").
-spec h1(
    list(lustre@vdom@vattr:attribute(SZZ)),
    list(lustre@vdom@vnode:element(SZZ))
) -> lustre@vdom@vnode:element(SZZ).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 118).
?DOC("\n").
-spec h2(
    list(lustre@vdom@vattr:attribute(TAF)),
    list(lustre@vdom@vnode:element(TAF))
) -> lustre@vdom@vnode:element(TAF).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 126).
?DOC("\n").
-spec h3(
    list(lustre@vdom@vattr:attribute(TAL)),
    list(lustre@vdom@vnode:element(TAL))
) -> lustre@vdom@vnode:element(TAL).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 134).
?DOC("\n").
-spec h4(
    list(lustre@vdom@vattr:attribute(TAR)),
    list(lustre@vdom@vnode:element(TAR))
) -> lustre@vdom@vnode:element(TAR).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 142).
?DOC("\n").
-spec h5(
    list(lustre@vdom@vattr:attribute(TAX)),
    list(lustre@vdom@vnode:element(TAX))
) -> lustre@vdom@vnode:element(TAX).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 150).
?DOC("\n").
-spec h6(
    list(lustre@vdom@vattr:attribute(TBD)),
    list(lustre@vdom@vnode:element(TBD))
) -> lustre@vdom@vnode:element(TBD).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 158).
?DOC("\n").
-spec hgroup(
    list(lustre@vdom@vattr:attribute(TBJ)),
    list(lustre@vdom@vnode:element(TBJ))
) -> lustre@vdom@vnode:element(TBJ).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 166).
?DOC("\n").
-spec main(
    list(lustre@vdom@vattr:attribute(TBP)),
    list(lustre@vdom@vnode:element(TBP))
) -> lustre@vdom@vnode:element(TBP).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 174).
?DOC("\n").
-spec nav(
    list(lustre@vdom@vattr:attribute(TBV)),
    list(lustre@vdom@vnode:element(TBV))
) -> lustre@vdom@vnode:element(TBV).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 182).
?DOC("\n").
-spec section(
    list(lustre@vdom@vattr:attribute(TCB)),
    list(lustre@vdom@vnode:element(TCB))
) -> lustre@vdom@vnode:element(TCB).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 190).
?DOC("\n").
-spec search(
    list(lustre@vdom@vattr:attribute(TCH)),
    list(lustre@vdom@vnode:element(TCH))
) -> lustre@vdom@vnode:element(TCH).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 200).
?DOC("\n").
-spec blockquote(
    list(lustre@vdom@vattr:attribute(TCN)),
    list(lustre@vdom@vnode:element(TCN))
) -> lustre@vdom@vnode:element(TCN).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 208).
?DOC("\n").
-spec dd(
    list(lustre@vdom@vattr:attribute(TCT)),
    list(lustre@vdom@vnode:element(TCT))
) -> lustre@vdom@vnode:element(TCT).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 216).
?DOC("\n").
-spec 'div'(
    list(lustre@vdom@vattr:attribute(TCZ)),
    list(lustre@vdom@vnode:element(TCZ))
) -> lustre@vdom@vnode:element(TCZ).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 224).
?DOC("\n").
-spec dl(
    list(lustre@vdom@vattr:attribute(TDF)),
    list(lustre@vdom@vnode:element(TDF))
) -> lustre@vdom@vnode:element(TDF).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 232).
?DOC("\n").
-spec dt(
    list(lustre@vdom@vattr:attribute(TDL)),
    list(lustre@vdom@vnode:element(TDL))
) -> lustre@vdom@vnode:element(TDL).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 240).
?DOC("\n").
-spec figcaption(
    list(lustre@vdom@vattr:attribute(TDR)),
    list(lustre@vdom@vnode:element(TDR))
) -> lustre@vdom@vnode:element(TDR).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 248).
?DOC("\n").
-spec figure(
    list(lustre@vdom@vattr:attribute(TDX)),
    list(lustre@vdom@vnode:element(TDX))
) -> lustre@vdom@vnode:element(TDX).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 256).
?DOC("\n").
-spec hr(list(lustre@vdom@vattr:attribute(TED))) -> lustre@vdom@vnode:element(TED).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 261).
?DOC("\n").
-spec li(
    list(lustre@vdom@vattr:attribute(TEH)),
    list(lustre@vdom@vnode:element(TEH))
) -> lustre@vdom@vnode:element(TEH).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 269).
?DOC("\n").
-spec menu(
    list(lustre@vdom@vattr:attribute(TEN)),
    list(lustre@vdom@vnode:element(TEN))
) -> lustre@vdom@vnode:element(TEN).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 277).
?DOC("\n").
-spec ol(
    list(lustre@vdom@vattr:attribute(TET)),
    list(lustre@vdom@vnode:element(TET))
) -> lustre@vdom@vnode:element(TET).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 285).
?DOC("\n").
-spec p(
    list(lustre@vdom@vattr:attribute(TEZ)),
    list(lustre@vdom@vnode:element(TEZ))
) -> lustre@vdom@vnode:element(TEZ).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 293).
?DOC("\n").
-spec pre(
    list(lustre@vdom@vattr:attribute(TFF)),
    list(lustre@vdom@vnode:element(TFF))
) -> lustre@vdom@vnode:element(TFF).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 301).
?DOC("\n").
-spec ul(
    list(lustre@vdom@vattr:attribute(TFL)),
    list(lustre@vdom@vnode:element(TFL))
) -> lustre@vdom@vnode:element(TFL).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 311).
?DOC("\n").
-spec a(
    list(lustre@vdom@vattr:attribute(TFR)),
    list(lustre@vdom@vnode:element(TFR))
) -> lustre@vdom@vnode:element(TFR).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 319).
?DOC("\n").
-spec abbr(
    list(lustre@vdom@vattr:attribute(TFX)),
    list(lustre@vdom@vnode:element(TFX))
) -> lustre@vdom@vnode:element(TFX).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 327).
?DOC("\n").
-spec b(
    list(lustre@vdom@vattr:attribute(TGD)),
    list(lustre@vdom@vnode:element(TGD))
) -> lustre@vdom@vnode:element(TGD).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 335).
?DOC("\n").
-spec bdi(
    list(lustre@vdom@vattr:attribute(TGJ)),
    list(lustre@vdom@vnode:element(TGJ))
) -> lustre@vdom@vnode:element(TGJ).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 343).
?DOC("\n").
-spec bdo(
    list(lustre@vdom@vattr:attribute(TGP)),
    list(lustre@vdom@vnode:element(TGP))
) -> lustre@vdom@vnode:element(TGP).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 351).
?DOC("\n").
-spec br(list(lustre@vdom@vattr:attribute(TGV))) -> lustre@vdom@vnode:element(TGV).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 356).
?DOC("\n").
-spec cite(
    list(lustre@vdom@vattr:attribute(TGZ)),
    list(lustre@vdom@vnode:element(TGZ))
) -> lustre@vdom@vnode:element(TGZ).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 364).
?DOC("\n").
-spec code(
    list(lustre@vdom@vattr:attribute(THF)),
    list(lustre@vdom@vnode:element(THF))
) -> lustre@vdom@vnode:element(THF).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 372).
?DOC("\n").
-spec data(
    list(lustre@vdom@vattr:attribute(THL)),
    list(lustre@vdom@vnode:element(THL))
) -> lustre@vdom@vnode:element(THL).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 380).
?DOC("\n").
-spec dfn(
    list(lustre@vdom@vattr:attribute(THR)),
    list(lustre@vdom@vnode:element(THR))
) -> lustre@vdom@vnode:element(THR).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 388).
?DOC("\n").
-spec em(
    list(lustre@vdom@vattr:attribute(THX)),
    list(lustre@vdom@vnode:element(THX))
) -> lustre@vdom@vnode:element(THX).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 396).
?DOC("\n").
-spec i(
    list(lustre@vdom@vattr:attribute(TID)),
    list(lustre@vdom@vnode:element(TID))
) -> lustre@vdom@vnode:element(TID).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 404).
?DOC("\n").
-spec kbd(
    list(lustre@vdom@vattr:attribute(TIJ)),
    list(lustre@vdom@vnode:element(TIJ))
) -> lustre@vdom@vnode:element(TIJ).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 412).
?DOC("\n").
-spec mark(
    list(lustre@vdom@vattr:attribute(TIP)),
    list(lustre@vdom@vnode:element(TIP))
) -> lustre@vdom@vnode:element(TIP).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 420).
?DOC("\n").
-spec q(
    list(lustre@vdom@vattr:attribute(TIV)),
    list(lustre@vdom@vnode:element(TIV))
) -> lustre@vdom@vnode:element(TIV).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 428).
?DOC("\n").
-spec rp(
    list(lustre@vdom@vattr:attribute(TJB)),
    list(lustre@vdom@vnode:element(TJB))
) -> lustre@vdom@vnode:element(TJB).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 436).
?DOC("\n").
-spec rt(
    list(lustre@vdom@vattr:attribute(TJH)),
    list(lustre@vdom@vnode:element(TJH))
) -> lustre@vdom@vnode:element(TJH).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 444).
?DOC("\n").
-spec ruby(
    list(lustre@vdom@vattr:attribute(TJN)),
    list(lustre@vdom@vnode:element(TJN))
) -> lustre@vdom@vnode:element(TJN).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 452).
?DOC("\n").
-spec s(
    list(lustre@vdom@vattr:attribute(TJT)),
    list(lustre@vdom@vnode:element(TJT))
) -> lustre@vdom@vnode:element(TJT).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 460).
?DOC("\n").
-spec samp(
    list(lustre@vdom@vattr:attribute(TJZ)),
    list(lustre@vdom@vnode:element(TJZ))
) -> lustre@vdom@vnode:element(TJZ).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 468).
?DOC("\n").
-spec small(
    list(lustre@vdom@vattr:attribute(TKF)),
    list(lustre@vdom@vnode:element(TKF))
) -> lustre@vdom@vnode:element(TKF).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 476).
?DOC("\n").
-spec span(
    list(lustre@vdom@vattr:attribute(TKL)),
    list(lustre@vdom@vnode:element(TKL))
) -> lustre@vdom@vnode:element(TKL).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 484).
?DOC("\n").
-spec strong(
    list(lustre@vdom@vattr:attribute(TKR)),
    list(lustre@vdom@vnode:element(TKR))
) -> lustre@vdom@vnode:element(TKR).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 492).
?DOC("\n").
-spec sub(
    list(lustre@vdom@vattr:attribute(TKX)),
    list(lustre@vdom@vnode:element(TKX))
) -> lustre@vdom@vnode:element(TKX).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 500).
?DOC("\n").
-spec sup(
    list(lustre@vdom@vattr:attribute(TLD)),
    list(lustre@vdom@vnode:element(TLD))
) -> lustre@vdom@vnode:element(TLD).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 508).
?DOC("\n").
-spec time(
    list(lustre@vdom@vattr:attribute(TLJ)),
    list(lustre@vdom@vnode:element(TLJ))
) -> lustre@vdom@vnode:element(TLJ).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 516).
?DOC("\n").
-spec u(
    list(lustre@vdom@vattr:attribute(TLP)),
    list(lustre@vdom@vnode:element(TLP))
) -> lustre@vdom@vnode:element(TLP).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 524).
?DOC("\n").
-spec var(
    list(lustre@vdom@vattr:attribute(TLV)),
    list(lustre@vdom@vnode:element(TLV))
) -> lustre@vdom@vnode:element(TLV).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 532).
?DOC("\n").
-spec wbr(list(lustre@vdom@vattr:attribute(TMB))) -> lustre@vdom@vnode:element(TMB).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 539).
?DOC("\n").
-spec area(list(lustre@vdom@vattr:attribute(TMF))) -> lustre@vdom@vnode:element(TMF).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 544).
?DOC("\n").
-spec audio(
    list(lustre@vdom@vattr:attribute(TMJ)),
    list(lustre@vdom@vnode:element(TMJ))
) -> lustre@vdom@vnode:element(TMJ).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 552).
?DOC("\n").
-spec img(list(lustre@vdom@vattr:attribute(TMP))) -> lustre@vdom@vnode:element(TMP).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 558).
?DOC(" Used with <area> elements to define an image map (a clickable link area).\n").
-spec map(
    list(lustre@vdom@vattr:attribute(TMT)),
    list(lustre@vdom@vnode:element(TMT))
) -> lustre@vdom@vnode:element(TMT).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 566).
?DOC("\n").
-spec track(list(lustre@vdom@vattr:attribute(TMZ))) -> lustre@vdom@vnode:element(TMZ).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 571).
?DOC("\n").
-spec video(
    list(lustre@vdom@vattr:attribute(TND)),
    list(lustre@vdom@vnode:element(TND))
) -> lustre@vdom@vnode:element(TND).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 581).
?DOC("\n").
-spec embed(list(lustre@vdom@vattr:attribute(TNJ))) -> lustre@vdom@vnode:element(TNJ).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 586).
?DOC("\n").
-spec iframe(list(lustre@vdom@vattr:attribute(TNN))) -> lustre@vdom@vnode:element(TNN).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 591).
?DOC("\n").
-spec object(list(lustre@vdom@vattr:attribute(TNR))) -> lustre@vdom@vnode:element(TNR).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 596).
?DOC("\n").
-spec picture(
    list(lustre@vdom@vattr:attribute(TNV)),
    list(lustre@vdom@vnode:element(TNV))
) -> lustre@vdom@vnode:element(TNV).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 604).
?DOC("\n").
-spec portal(list(lustre@vdom@vattr:attribute(TOB))) -> lustre@vdom@vnode:element(TOB).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 609).
?DOC("\n").
-spec source(list(lustre@vdom@vattr:attribute(TOF))) -> lustre@vdom@vnode:element(TOF).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 616).
?DOC("\n").
-spec svg(
    list(lustre@vdom@vattr:attribute(TOJ)),
    list(lustre@vdom@vnode:element(TOJ))
) -> lustre@vdom@vnode:element(TOJ).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/html.gleam", 624).
?DOC("\n").
-spec math(
    list(lustre@vdom@vattr:attribute(TOP)),
    list(lustre@vdom@vnode:element(TOP))
) -> lustre@vdom@vnode:element(TOP).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 634).
?DOC("\n").
-spec canvas(list(lustre@vdom@vattr:attribute(TOV))) -> lustre@vdom@vnode:element(TOV).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 639).
?DOC("\n").
-spec noscript(
    list(lustre@vdom@vattr:attribute(TOZ)),
    list(lustre@vdom@vnode:element(TOZ))
) -> lustre@vdom@vnode:element(TOZ).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 647).
?DOC("\n").
-spec script(list(lustre@vdom@vattr:attribute(TPF)), binary()) -> lustre@vdom@vnode:element(TPF).
script(Attrs, Js) ->
    lustre@element:unsafe_raw_html(<<""/utf8>>, <<"script"/utf8>>, Attrs, Js).

-file("src/lustre/element/html.gleam", 654).
?DOC("\n").
-spec del(
    list(lustre@vdom@vattr:attribute(TPJ)),
    list(lustre@vdom@vnode:element(TPJ))
) -> lustre@vdom@vnode:element(TPJ).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 662).
?DOC("\n").
-spec ins(
    list(lustre@vdom@vattr:attribute(TPP)),
    list(lustre@vdom@vnode:element(TPP))
) -> lustre@vdom@vnode:element(TPP).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 672).
?DOC("\n").
-spec caption(
    list(lustre@vdom@vattr:attribute(TPV)),
    list(lustre@vdom@vnode:element(TPV))
) -> lustre@vdom@vnode:element(TPV).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 680).
?DOC("\n").
-spec col(list(lustre@vdom@vattr:attribute(TQB))) -> lustre@vdom@vnode:element(TQB).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 685).
?DOC("\n").
-spec colgroup(
    list(lustre@vdom@vattr:attribute(TQF)),
    list(lustre@vdom@vnode:element(TQF))
) -> lustre@vdom@vnode:element(TQF).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 693).
?DOC("\n").
-spec table(
    list(lustre@vdom@vattr:attribute(TQL)),
    list(lustre@vdom@vnode:element(TQL))
) -> lustre@vdom@vnode:element(TQL).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 701).
?DOC("\n").
-spec tbody(
    list(lustre@vdom@vattr:attribute(TQR)),
    list(lustre@vdom@vnode:element(TQR))
) -> lustre@vdom@vnode:element(TQR).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 709).
?DOC("\n").
-spec td(
    list(lustre@vdom@vattr:attribute(TQX)),
    list(lustre@vdom@vnode:element(TQX))
) -> lustre@vdom@vnode:element(TQX).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 717).
?DOC("\n").
-spec tfoot(
    list(lustre@vdom@vattr:attribute(TRD)),
    list(lustre@vdom@vnode:element(TRD))
) -> lustre@vdom@vnode:element(TRD).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 725).
?DOC("\n").
-spec th(
    list(lustre@vdom@vattr:attribute(TRJ)),
    list(lustre@vdom@vnode:element(TRJ))
) -> lustre@vdom@vnode:element(TRJ).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 733).
?DOC("\n").
-spec thead(
    list(lustre@vdom@vattr:attribute(TRP)),
    list(lustre@vdom@vnode:element(TRP))
) -> lustre@vdom@vnode:element(TRP).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 741).
?DOC("\n").
-spec tr(
    list(lustre@vdom@vattr:attribute(TRV)),
    list(lustre@vdom@vnode:element(TRV))
) -> lustre@vdom@vnode:element(TRV).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 751).
?DOC("\n").
-spec button(
    list(lustre@vdom@vattr:attribute(TSB)),
    list(lustre@vdom@vnode:element(TSB))
) -> lustre@vdom@vnode:element(TSB).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 759).
?DOC("\n").
-spec datalist(
    list(lustre@vdom@vattr:attribute(TSH)),
    list(lustre@vdom@vnode:element(TSH))
) -> lustre@vdom@vnode:element(TSH).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 767).
?DOC("\n").
-spec fieldset(
    list(lustre@vdom@vattr:attribute(TSN)),
    list(lustre@vdom@vnode:element(TSN))
) -> lustre@vdom@vnode:element(TSN).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 775).
?DOC("\n").
-spec form(
    list(lustre@vdom@vattr:attribute(TST)),
    list(lustre@vdom@vnode:element(TST))
) -> lustre@vdom@vnode:element(TST).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 783).
?DOC("\n").
-spec input(list(lustre@vdom@vattr:attribute(TSZ))) -> lustre@vdom@vnode:element(TSZ).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-file("src/lustre/element/html.gleam", 788).
?DOC("\n").
-spec label(
    list(lustre@vdom@vattr:attribute(TTD)),
    list(lustre@vdom@vnode:element(TTD))
) -> lustre@vdom@vnode:element(TTD).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 796).
?DOC("\n").
-spec legend(
    list(lustre@vdom@vattr:attribute(TTJ)),
    list(lustre@vdom@vnode:element(TTJ))
) -> lustre@vdom@vnode:element(TTJ).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 804).
?DOC("\n").
-spec meter(
    list(lustre@vdom@vattr:attribute(TTP)),
    list(lustre@vdom@vnode:element(TTP))
) -> lustre@vdom@vnode:element(TTP).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 812).
?DOC("\n").
-spec optgroup(
    list(lustre@vdom@vattr:attribute(TTV)),
    list(lustre@vdom@vnode:element(TTV))
) -> lustre@vdom@vnode:element(TTV).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 820).
?DOC("\n").
-spec option(list(lustre@vdom@vattr:attribute(TUB)), binary()) -> lustre@vdom@vnode:element(TUB).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-file("src/lustre/element/html.gleam", 825).
?DOC("\n").
-spec output(
    list(lustre@vdom@vattr:attribute(TUF)),
    list(lustre@vdom@vnode:element(TUF))
) -> lustre@vdom@vnode:element(TUF).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 833).
?DOC("\n").
-spec progress(
    list(lustre@vdom@vattr:attribute(TUL)),
    list(lustre@vdom@vnode:element(TUL))
) -> lustre@vdom@vnode:element(TUL).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 841).
?DOC("\n").
-spec select(
    list(lustre@vdom@vattr:attribute(TUR)),
    list(lustre@vdom@vnode:element(TUR))
) -> lustre@vdom@vnode:element(TUR).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 849).
?DOC("\n").
-spec textarea(list(lustre@vdom@vattr:attribute(TUX)), binary()) -> lustre@vdom@vnode:element(TUX).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        [lustre@attribute:property(<<"value"/utf8>>, gleam@json:string(Content)) |
            Attrs],
        [lustre@element:text(Content)]
    ).

-file("src/lustre/element/html.gleam", 860).
?DOC("\n").
-spec details(
    list(lustre@vdom@vattr:attribute(TVB)),
    list(lustre@vdom@vnode:element(TVB))
) -> lustre@vdom@vnode:element(TVB).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 868).
?DOC("\n").
-spec dialog(
    list(lustre@vdom@vattr:attribute(TVH)),
    list(lustre@vdom@vnode:element(TVH))
) -> lustre@vdom@vnode:element(TVH).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 876).
?DOC("\n").
-spec summary(
    list(lustre@vdom@vattr:attribute(TVN)),
    list(lustre@vdom@vnode:element(TVN))
) -> lustre@vdom@vnode:element(TVN).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-file("src/lustre/element/html.gleam", 886).
?DOC("\n").
-spec slot(
    list(lustre@vdom@vattr:attribute(TVT)),
    list(lustre@vdom@vnode:element(TVT))
) -> lustre@vdom@vnode:element(TVT).
slot(Attrs, Fallback) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, Fallback).

-file("src/lustre/element/html.gleam", 894).
?DOC("\n").
-spec template(
    list(lustre@vdom@vattr:attribute(TVZ)),
    list(lustre@vdom@vnode:element(TVZ))
) -> lustre@vdom@vnode:element(TVZ).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
