-module(marceau).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([extension_to_mime_type/1, mime_type_to_extensions/1]).

-file("/Users/louis/src/gleam/marceau/src/marceau.gleam", 1).
-spec extension_to_mime_type(binary()) -> binary().
extension_to_mime_type(Extension) ->
    case Extension of
        <<"123"/utf8>> ->
            <<"application/vnd.lotus-1-2-3"/utf8>>;

        <<"3dml"/utf8>> ->
            <<"text/vnd.in3d.3dml"/utf8>>;

        <<"3ds"/utf8>> ->
            <<"image/x-3ds"/utf8>>;

        <<"3g2"/utf8>> ->
            <<"video/3gpp2"/utf8>>;

        <<"3gp"/utf8>> ->
            <<"video/3gpp"/utf8>>;

        <<"7z"/utf8>> ->
            <<"application/x-7z-compressed"/utf8>>;

        <<"aab"/utf8>> ->
            <<"application/x-authorware-bin"/utf8>>;

        <<"aac"/utf8>> ->
            <<"audio/x-aac"/utf8>>;

        <<"aam"/utf8>> ->
            <<"application/x-authorware-map"/utf8>>;

        <<"aas"/utf8>> ->
            <<"application/x-authorware-seg"/utf8>>;

        <<"abw"/utf8>> ->
            <<"application/x-abiword"/utf8>>;

        <<"ac"/utf8>> ->
            <<"application/pkix-attr-cert"/utf8>>;

        <<"acc"/utf8>> ->
            <<"application/vnd.americandynamics.acc"/utf8>>;

        <<"ace"/utf8>> ->
            <<"application/x-ace-compressed"/utf8>>;

        <<"acu"/utf8>> ->
            <<"application/vnd.acucobol"/utf8>>;

        <<"acutc"/utf8>> ->
            <<"application/vnd.acucorp"/utf8>>;

        <<"adp"/utf8>> ->
            <<"audio/adpcm"/utf8>>;

        <<"aep"/utf8>> ->
            <<"application/vnd.audiograph"/utf8>>;

        <<"afm"/utf8>> ->
            <<"application/x-font-type1"/utf8>>;

        <<"afp"/utf8>> ->
            <<"application/vnd.ibm.modcap"/utf8>>;

        <<"ahead"/utf8>> ->
            <<"application/vnd.ahead.space"/utf8>>;

        <<"ai"/utf8>> ->
            <<"application/postscript"/utf8>>;

        <<"aif"/utf8>> ->
            <<"audio/x-aiff"/utf8>>;

        <<"aifc"/utf8>> ->
            <<"audio/x-aiff"/utf8>>;

        <<"aiff"/utf8>> ->
            <<"audio/x-aiff"/utf8>>;

        <<"air"/utf8>> ->
            <<"application/vnd.adobe.air-application-installer-package+zip"/utf8>>;

        <<"ait"/utf8>> ->
            <<"application/vnd.dvb.ait"/utf8>>;

        <<"ami"/utf8>> ->
            <<"application/vnd.amiga.ami"/utf8>>;

        <<"apk"/utf8>> ->
            <<"application/vnd.android.package-archive"/utf8>>;

        <<"appcache"/utf8>> ->
            <<"text/cache-manifest"/utf8>>;

        <<"application"/utf8>> ->
            <<"application/x-ms-application"/utf8>>;

        <<"apr"/utf8>> ->
            <<"application/vnd.lotus-approach"/utf8>>;

        <<"arc"/utf8>> ->
            <<"application/x-freearc"/utf8>>;

        <<"asc"/utf8>> ->
            <<"application/pgp-signature"/utf8>>;

        <<"asf"/utf8>> ->
            <<"video/x-ms-asf"/utf8>>;

        <<"asm"/utf8>> ->
            <<"text/x-asm"/utf8>>;

        <<"aso"/utf8>> ->
            <<"application/vnd.accpac.simply.aso"/utf8>>;

        <<"asx"/utf8>> ->
            <<"video/x-ms-asf"/utf8>>;

        <<"atc"/utf8>> ->
            <<"application/vnd.acucorp"/utf8>>;

        <<"atom"/utf8>> ->
            <<"application/atom+xml"/utf8>>;

        <<"atomcat"/utf8>> ->
            <<"application/atomcat+xml"/utf8>>;

        <<"atomsvc"/utf8>> ->
            <<"application/atomsvc+xml"/utf8>>;

        <<"atx"/utf8>> ->
            <<"application/vnd.antix.game-component"/utf8>>;

        <<"au"/utf8>> ->
            <<"audio/basic"/utf8>>;

        <<"avi"/utf8>> ->
            <<"video/x-msvideo"/utf8>>;

        <<"avif"/utf8>> ->
            <<"image/avif"/utf8>>;

        <<"aw"/utf8>> ->
            <<"application/applixware"/utf8>>;

        <<"azf"/utf8>> ->
            <<"application/vnd.airzip.filesecure.azf"/utf8>>;

        <<"azs"/utf8>> ->
            <<"application/vnd.airzip.filesecure.azs"/utf8>>;

        <<"azw"/utf8>> ->
            <<"application/vnd.amazon.ebook"/utf8>>;

        <<"bat"/utf8>> ->
            <<"application/x-msdownload"/utf8>>;

        <<"bcpio"/utf8>> ->
            <<"application/x-bcpio"/utf8>>;

        <<"bdf"/utf8>> ->
            <<"application/x-font-bdf"/utf8>>;

        <<"bdm"/utf8>> ->
            <<"application/vnd.syncml.dm+wbxml"/utf8>>;

        <<"bed"/utf8>> ->
            <<"application/vnd.realvnc.bed"/utf8>>;

        <<"bh2"/utf8>> ->
            <<"application/vnd.fujitsu.oasysprs"/utf8>>;

        <<"bin"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"blb"/utf8>> ->
            <<"application/x-blorb"/utf8>>;

        <<"blorb"/utf8>> ->
            <<"application/x-blorb"/utf8>>;

        <<"bmi"/utf8>> ->
            <<"application/vnd.bmi"/utf8>>;

        <<"bmp"/utf8>> ->
            <<"image/bmp"/utf8>>;

        <<"book"/utf8>> ->
            <<"application/vnd.framemaker"/utf8>>;

        <<"box"/utf8>> ->
            <<"application/vnd.previewsystems.box"/utf8>>;

        <<"boz"/utf8>> ->
            <<"application/x-bzip2"/utf8>>;

        <<"bpk"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"btif"/utf8>> ->
            <<"image/prs.btif"/utf8>>;

        <<"bz"/utf8>> ->
            <<"application/x-bzip"/utf8>>;

        <<"bz2"/utf8>> ->
            <<"application/x-bzip2"/utf8>>;

        <<"c"/utf8>> ->
            <<"text/x-c"/utf8>>;

        <<"c11amc"/utf8>> ->
            <<"application/vnd.cluetrust.cartomobile-config"/utf8>>;

        <<"c11amz"/utf8>> ->
            <<"application/vnd.cluetrust.cartomobile-config-pkg"/utf8>>;

        <<"c4d"/utf8>> ->
            <<"application/vnd.clonk.c4group"/utf8>>;

        <<"c4f"/utf8>> ->
            <<"application/vnd.clonk.c4group"/utf8>>;

        <<"c4g"/utf8>> ->
            <<"application/vnd.clonk.c4group"/utf8>>;

        <<"c4p"/utf8>> ->
            <<"application/vnd.clonk.c4group"/utf8>>;

        <<"c4u"/utf8>> ->
            <<"application/vnd.clonk.c4group"/utf8>>;

        <<"cab"/utf8>> ->
            <<"application/vnd.ms-cab-compressed"/utf8>>;

        <<"caf"/utf8>> ->
            <<"audio/x-caf"/utf8>>;

        <<"cap"/utf8>> ->
            <<"application/vnd.tcpdump.pcap"/utf8>>;

        <<"car"/utf8>> ->
            <<"application/vnd.curl.car"/utf8>>;

        <<"cat"/utf8>> ->
            <<"application/vnd.ms-pki.seccat"/utf8>>;

        <<"cb7"/utf8>> ->
            <<"application/x-cbr"/utf8>>;

        <<"cba"/utf8>> ->
            <<"application/x-cbr"/utf8>>;

        <<"cbr"/utf8>> ->
            <<"application/x-cbr"/utf8>>;

        <<"cbt"/utf8>> ->
            <<"application/x-cbr"/utf8>>;

        <<"cbz"/utf8>> ->
            <<"application/x-cbr"/utf8>>;

        <<"cc"/utf8>> ->
            <<"text/x-c"/utf8>>;

        <<"cct"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"ccxml"/utf8>> ->
            <<"application/ccxml+xml"/utf8>>;

        <<"cdbcmsg"/utf8>> ->
            <<"application/vnd.contact.cmsg"/utf8>>;

        <<"cdf"/utf8>> ->
            <<"application/x-netcdf"/utf8>>;

        <<"cdkey"/utf8>> ->
            <<"application/vnd.mediastation.cdkey"/utf8>>;

        <<"cdmia"/utf8>> ->
            <<"application/cdmi-capability"/utf8>>;

        <<"cdmic"/utf8>> ->
            <<"application/cdmi-container"/utf8>>;

        <<"cdmid"/utf8>> ->
            <<"application/cdmi-domain"/utf8>>;

        <<"cdmio"/utf8>> ->
            <<"application/cdmi-object"/utf8>>;

        <<"cdmiq"/utf8>> ->
            <<"application/cdmi-queue"/utf8>>;

        <<"cdx"/utf8>> ->
            <<"chemical/x-cdx"/utf8>>;

        <<"cdxml"/utf8>> ->
            <<"application/vnd.chemdraw+xml"/utf8>>;

        <<"cdy"/utf8>> ->
            <<"application/vnd.cinderella"/utf8>>;

        <<"cer"/utf8>> ->
            <<"application/pkix-cert"/utf8>>;

        <<"cfs"/utf8>> ->
            <<"application/x-cfs-compressed"/utf8>>;

        <<"cgm"/utf8>> ->
            <<"image/cgm"/utf8>>;

        <<"chat"/utf8>> ->
            <<"application/x-chat"/utf8>>;

        <<"chm"/utf8>> ->
            <<"application/vnd.ms-htmlhelp"/utf8>>;

        <<"chrt"/utf8>> ->
            <<"application/vnd.kde.kchart"/utf8>>;

        <<"cif"/utf8>> ->
            <<"chemical/x-cif"/utf8>>;

        <<"cii"/utf8>> ->
            <<"application/vnd.anser-web-certificate-issue-initiation"/utf8>>;

        <<"cil"/utf8>> ->
            <<"application/vnd.ms-artgalry"/utf8>>;

        <<"cla"/utf8>> ->
            <<"application/vnd.claymore"/utf8>>;

        <<"class"/utf8>> ->
            <<"application/java-vm"/utf8>>;

        <<"clkk"/utf8>> ->
            <<"application/vnd.crick.clicker.keyboard"/utf8>>;

        <<"clkp"/utf8>> ->
            <<"application/vnd.crick.clicker.palette"/utf8>>;

        <<"clkt"/utf8>> ->
            <<"application/vnd.crick.clicker.template"/utf8>>;

        <<"clkw"/utf8>> ->
            <<"application/vnd.crick.clicker.wordbank"/utf8>>;

        <<"clkx"/utf8>> ->
            <<"application/vnd.crick.clicker"/utf8>>;

        <<"clp"/utf8>> ->
            <<"application/x-msclip"/utf8>>;

        <<"cmc"/utf8>> ->
            <<"application/vnd.cosmocaller"/utf8>>;

        <<"cmdf"/utf8>> ->
            <<"chemical/x-cmdf"/utf8>>;

        <<"cml"/utf8>> ->
            <<"chemical/x-cml"/utf8>>;

        <<"cmp"/utf8>> ->
            <<"application/vnd.yellowriver-custom-menu"/utf8>>;

        <<"cmx"/utf8>> ->
            <<"image/x-cmx"/utf8>>;

        <<"cod"/utf8>> ->
            <<"application/vnd.rim.cod"/utf8>>;

        <<"com"/utf8>> ->
            <<"application/x-msdownload"/utf8>>;

        <<"conf"/utf8>> ->
            <<"text/plain"/utf8>>;

        <<"cpio"/utf8>> ->
            <<"application/x-cpio"/utf8>>;

        <<"cpp"/utf8>> ->
            <<"text/x-c"/utf8>>;

        <<"cpt"/utf8>> ->
            <<"application/mac-compactpro"/utf8>>;

        <<"crd"/utf8>> ->
            <<"application/x-mscardfile"/utf8>>;

        <<"crl"/utf8>> ->
            <<"application/pkix-crl"/utf8>>;

        <<"crt"/utf8>> ->
            <<"application/x-x509-ca-cert"/utf8>>;

        <<"cryptonote"/utf8>> ->
            <<"application/vnd.rig.cryptonote"/utf8>>;

        <<"csh"/utf8>> ->
            <<"application/x-csh"/utf8>>;

        <<"csml"/utf8>> ->
            <<"chemical/x-csml"/utf8>>;

        <<"csp"/utf8>> ->
            <<"application/vnd.commonspace"/utf8>>;

        <<"css"/utf8>> ->
            <<"text/css"/utf8>>;

        <<"cst"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"csv"/utf8>> ->
            <<"text/csv"/utf8>>;

        <<"cu"/utf8>> ->
            <<"application/cu-seeme"/utf8>>;

        <<"curl"/utf8>> ->
            <<"text/vnd.curl"/utf8>>;

        <<"cww"/utf8>> ->
            <<"application/prs.cww"/utf8>>;

        <<"cxt"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"cxx"/utf8>> ->
            <<"text/x-c"/utf8>>;

        <<"dae"/utf8>> ->
            <<"model/vnd.collada+xml"/utf8>>;

        <<"daf"/utf8>> ->
            <<"application/vnd.mobius.daf"/utf8>>;

        <<"dart"/utf8>> ->
            <<"application/vnd.dart"/utf8>>;

        <<"dataless"/utf8>> ->
            <<"application/vnd.fdsn.seed"/utf8>>;

        <<"davmount"/utf8>> ->
            <<"application/davmount+xml"/utf8>>;

        <<"dbk"/utf8>> ->
            <<"application/docbook+xml"/utf8>>;

        <<"dcr"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"dcurl"/utf8>> ->
            <<"text/vnd.curl.dcurl"/utf8>>;

        <<"dd2"/utf8>> ->
            <<"application/vnd.oma.dd2+xml"/utf8>>;

        <<"ddd"/utf8>> ->
            <<"application/vnd.fujixerox.ddd"/utf8>>;

        <<"deb"/utf8>> ->
            <<"application/x-debian-package"/utf8>>;

        <<"def"/utf8>> ->
            <<"text/plain"/utf8>>;

        <<"deploy"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"der"/utf8>> ->
            <<"application/x-x509-ca-cert"/utf8>>;

        <<"dfac"/utf8>> ->
            <<"application/vnd.dreamfactory"/utf8>>;

        <<"dgc"/utf8>> ->
            <<"application/x-dgc-compressed"/utf8>>;

        <<"dic"/utf8>> ->
            <<"text/x-c"/utf8>>;

        <<"dir"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"dis"/utf8>> ->
            <<"application/vnd.mobius.dis"/utf8>>;

        <<"dist"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"distz"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"djv"/utf8>> ->
            <<"image/vnd.djvu"/utf8>>;

        <<"djvu"/utf8>> ->
            <<"image/vnd.djvu"/utf8>>;

        <<"dll"/utf8>> ->
            <<"application/x-msdownload"/utf8>>;

        <<"dmg"/utf8>> ->
            <<"application/x-apple-diskimage"/utf8>>;

        <<"dmp"/utf8>> ->
            <<"application/vnd.tcpdump.pcap"/utf8>>;

        <<"dms"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"dna"/utf8>> ->
            <<"application/vnd.dna"/utf8>>;

        <<"doc"/utf8>> ->
            <<"application/msword"/utf8>>;

        <<"docm"/utf8>> ->
            <<"application/vnd.ms-word.document.macroenabled.12"/utf8>>;

        <<"docx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document"/utf8>>;

        <<"dot"/utf8>> ->
            <<"application/msword"/utf8>>;

        <<"dotm"/utf8>> ->
            <<"application/vnd.ms-word.template.macroenabled.12"/utf8>>;

        <<"dotx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.wordprocessingml.template"/utf8>>;

        <<"dp"/utf8>> ->
            <<"application/vnd.osgi.dp"/utf8>>;

        <<"dpg"/utf8>> ->
            <<"application/vnd.dpgraph"/utf8>>;

        <<"dra"/utf8>> ->
            <<"audio/vnd.dra"/utf8>>;

        <<"dsc"/utf8>> ->
            <<"text/prs.lines.tag"/utf8>>;

        <<"dssc"/utf8>> ->
            <<"application/dssc+der"/utf8>>;

        <<"dtb"/utf8>> ->
            <<"application/x-dtbook+xml"/utf8>>;

        <<"dtd"/utf8>> ->
            <<"application/xml-dtd"/utf8>>;

        <<"dts"/utf8>> ->
            <<"audio/vnd.dts"/utf8>>;

        <<"dtshd"/utf8>> ->
            <<"audio/vnd.dts.hd"/utf8>>;

        <<"dump"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"dvb"/utf8>> ->
            <<"video/vnd.dvb.file"/utf8>>;

        <<"dvi"/utf8>> ->
            <<"application/x-dvi"/utf8>>;

        <<"dwf"/utf8>> ->
            <<"model/vnd.dwf"/utf8>>;

        <<"dwg"/utf8>> ->
            <<"image/vnd.dwg"/utf8>>;

        <<"dxf"/utf8>> ->
            <<"image/vnd.dxf"/utf8>>;

        <<"dxp"/utf8>> ->
            <<"application/vnd.spotfire.dxp"/utf8>>;

        <<"dxr"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"ecelp4800"/utf8>> ->
            <<"audio/vnd.nuera.ecelp4800"/utf8>>;

        <<"ecelp7470"/utf8>> ->
            <<"audio/vnd.nuera.ecelp7470"/utf8>>;

        <<"ecelp9600"/utf8>> ->
            <<"audio/vnd.nuera.ecelp9600"/utf8>>;

        <<"ecma"/utf8>> ->
            <<"application/ecmascript"/utf8>>;

        <<"edm"/utf8>> ->
            <<"application/vnd.novadigm.edm"/utf8>>;

        <<"edx"/utf8>> ->
            <<"application/vnd.novadigm.edx"/utf8>>;

        <<"efif"/utf8>> ->
            <<"application/vnd.picsel"/utf8>>;

        <<"ei6"/utf8>> ->
            <<"application/vnd.pg.osasli"/utf8>>;

        <<"elc"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"emf"/utf8>> ->
            <<"application/x-msmetafile"/utf8>>;

        <<"eml"/utf8>> ->
            <<"message/rfc822"/utf8>>;

        <<"emma"/utf8>> ->
            <<"application/emma+xml"/utf8>>;

        <<"emz"/utf8>> ->
            <<"application/x-msmetafile"/utf8>>;

        <<"eol"/utf8>> ->
            <<"audio/vnd.digital-winds"/utf8>>;

        <<"eot"/utf8>> ->
            <<"application/vnd.ms-fontobject"/utf8>>;

        <<"eps"/utf8>> ->
            <<"application/postscript"/utf8>>;

        <<"epub"/utf8>> ->
            <<"application/epub+zip"/utf8>>;

        <<"es3"/utf8>> ->
            <<"application/vnd.eszigno3+xml"/utf8>>;

        <<"esa"/utf8>> ->
            <<"application/vnd.osgi.subsystem"/utf8>>;

        <<"esf"/utf8>> ->
            <<"application/vnd.epson.esf"/utf8>>;

        <<"et3"/utf8>> ->
            <<"application/vnd.eszigno3+xml"/utf8>>;

        <<"etx"/utf8>> ->
            <<"text/x-setext"/utf8>>;

        <<"eva"/utf8>> ->
            <<"application/x-eva"/utf8>>;

        <<"evy"/utf8>> ->
            <<"application/x-envoy"/utf8>>;

        <<"exe"/utf8>> ->
            <<"application/x-msdownload"/utf8>>;

        <<"exi"/utf8>> ->
            <<"application/exi"/utf8>>;

        <<"ext"/utf8>> ->
            <<"application/vnd.novadigm.ext"/utf8>>;

        <<"ez"/utf8>> ->
            <<"application/andrew-inset"/utf8>>;

        <<"ez2"/utf8>> ->
            <<"application/vnd.ezpix-album"/utf8>>;

        <<"ez3"/utf8>> ->
            <<"application/vnd.ezpix-package"/utf8>>;

        <<"f"/utf8>> ->
            <<"text/x-fortran"/utf8>>;

        <<"f4v"/utf8>> ->
            <<"video/x-f4v"/utf8>>;

        <<"f77"/utf8>> ->
            <<"text/x-fortran"/utf8>>;

        <<"f90"/utf8>> ->
            <<"text/x-fortran"/utf8>>;

        <<"fbs"/utf8>> ->
            <<"image/vnd.fastbidsheet"/utf8>>;

        <<"fcdt"/utf8>> ->
            <<"application/vnd.adobe.formscentral.fcdt"/utf8>>;

        <<"fcs"/utf8>> ->
            <<"application/vnd.isac.fcs"/utf8>>;

        <<"fdf"/utf8>> ->
            <<"application/vnd.fdf"/utf8>>;

        <<"fe_launch"/utf8>> ->
            <<"application/vnd.denovo.fcselayout-link"/utf8>>;

        <<"fg5"/utf8>> ->
            <<"application/vnd.fujitsu.oasysgp"/utf8>>;

        <<"fgd"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"fh"/utf8>> ->
            <<"image/x-freehand"/utf8>>;

        <<"fh4"/utf8>> ->
            <<"image/x-freehand"/utf8>>;

        <<"fh5"/utf8>> ->
            <<"image/x-freehand"/utf8>>;

        <<"fh7"/utf8>> ->
            <<"image/x-freehand"/utf8>>;

        <<"fhc"/utf8>> ->
            <<"image/x-freehand"/utf8>>;

        <<"fig"/utf8>> ->
            <<"application/x-xfig"/utf8>>;

        <<"flac"/utf8>> ->
            <<"audio/x-flac"/utf8>>;

        <<"fli"/utf8>> ->
            <<"video/x-fli"/utf8>>;

        <<"flo"/utf8>> ->
            <<"application/vnd.micrografx.flo"/utf8>>;

        <<"flv"/utf8>> ->
            <<"video/x-flv"/utf8>>;

        <<"flw"/utf8>> ->
            <<"application/vnd.kde.kivio"/utf8>>;

        <<"flx"/utf8>> ->
            <<"text/vnd.fmi.flexstor"/utf8>>;

        <<"fly"/utf8>> ->
            <<"text/vnd.fly"/utf8>>;

        <<"fm"/utf8>> ->
            <<"application/vnd.framemaker"/utf8>>;

        <<"fnc"/utf8>> ->
            <<"application/vnd.frogans.fnc"/utf8>>;

        <<"for"/utf8>> ->
            <<"text/x-fortran"/utf8>>;

        <<"fpx"/utf8>> ->
            <<"image/vnd.fpx"/utf8>>;

        <<"frame"/utf8>> ->
            <<"application/vnd.framemaker"/utf8>>;

        <<"fsc"/utf8>> ->
            <<"application/vnd.fsc.weblaunch"/utf8>>;

        <<"fst"/utf8>> ->
            <<"image/vnd.fst"/utf8>>;

        <<"ftc"/utf8>> ->
            <<"application/vnd.fluxtime.clip"/utf8>>;

        <<"fti"/utf8>> ->
            <<"application/vnd.anser-web-funds-transfer-initiation"/utf8>>;

        <<"fvt"/utf8>> ->
            <<"video/vnd.fvt"/utf8>>;

        <<"fxp"/utf8>> ->
            <<"application/vnd.adobe.fxp"/utf8>>;

        <<"fxpl"/utf8>> ->
            <<"application/vnd.adobe.fxp"/utf8>>;

        <<"fzs"/utf8>> ->
            <<"application/vnd.fuzzysheet"/utf8>>;

        <<"g2w"/utf8>> ->
            <<"application/vnd.geoplan"/utf8>>;

        <<"g3"/utf8>> ->
            <<"image/g3fax"/utf8>>;

        <<"g3w"/utf8>> ->
            <<"application/vnd.geospace"/utf8>>;

        <<"gac"/utf8>> ->
            <<"application/vnd.groove-account"/utf8>>;

        <<"gam"/utf8>> ->
            <<"application/x-tads"/utf8>>;

        <<"gbr"/utf8>> ->
            <<"application/rpki-ghostbusters"/utf8>>;

        <<"gca"/utf8>> ->
            <<"application/x-gca-compressed"/utf8>>;

        <<"gdl"/utf8>> ->
            <<"model/vnd.gdl"/utf8>>;

        <<"geo"/utf8>> ->
            <<"application/vnd.dynageo"/utf8>>;

        <<"gex"/utf8>> ->
            <<"application/vnd.geometry-explorer"/utf8>>;

        <<"ggb"/utf8>> ->
            <<"application/vnd.geogebra.file"/utf8>>;

        <<"ggs"/utf8>> ->
            <<"application/vnd.geogebra.slides"/utf8>>;

        <<"ggt"/utf8>> ->
            <<"application/vnd.geogebra.tool"/utf8>>;

        <<"ghf"/utf8>> ->
            <<"application/vnd.groove-help"/utf8>>;

        <<"gif"/utf8>> ->
            <<"image/gif"/utf8>>;

        <<"gim"/utf8>> ->
            <<"application/vnd.groove-identity-message"/utf8>>;

        <<"gml"/utf8>> ->
            <<"application/gml+xml"/utf8>>;

        <<"gmx"/utf8>> ->
            <<"application/vnd.gmx"/utf8>>;

        <<"gnumeric"/utf8>> ->
            <<"application/x-gnumeric"/utf8>>;

        <<"gph"/utf8>> ->
            <<"application/vnd.flographit"/utf8>>;

        <<"gpx"/utf8>> ->
            <<"application/gpx+xml"/utf8>>;

        <<"gqf"/utf8>> ->
            <<"application/vnd.grafeq"/utf8>>;

        <<"gqs"/utf8>> ->
            <<"application/vnd.grafeq"/utf8>>;

        <<"gram"/utf8>> ->
            <<"application/srgs"/utf8>>;

        <<"gramps"/utf8>> ->
            <<"application/x-gramps-xml"/utf8>>;

        <<"gre"/utf8>> ->
            <<"application/vnd.geometry-explorer"/utf8>>;

        <<"grv"/utf8>> ->
            <<"application/vnd.groove-injector"/utf8>>;

        <<"grxml"/utf8>> ->
            <<"application/srgs+xml"/utf8>>;

        <<"gsf"/utf8>> ->
            <<"application/x-font-ghostscript"/utf8>>;

        <<"gtar"/utf8>> ->
            <<"application/x-gtar"/utf8>>;

        <<"gtm"/utf8>> ->
            <<"application/vnd.groove-tool-message"/utf8>>;

        <<"gtw"/utf8>> ->
            <<"model/vnd.gtw"/utf8>>;

        <<"gv"/utf8>> ->
            <<"text/vnd.graphviz"/utf8>>;

        <<"gxf"/utf8>> ->
            <<"application/gxf"/utf8>>;

        <<"gxt"/utf8>> ->
            <<"application/vnd.geonext"/utf8>>;

        <<"h"/utf8>> ->
            <<"text/x-c"/utf8>>;

        <<"h261"/utf8>> ->
            <<"video/h261"/utf8>>;

        <<"h263"/utf8>> ->
            <<"video/h263"/utf8>>;

        <<"h264"/utf8>> ->
            <<"video/h264"/utf8>>;

        <<"hal"/utf8>> ->
            <<"application/vnd.hal+xml"/utf8>>;

        <<"hbci"/utf8>> ->
            <<"application/vnd.hbci"/utf8>>;

        <<"hdf"/utf8>> ->
            <<"application/x-hdf"/utf8>>;

        <<"hh"/utf8>> ->
            <<"text/x-c"/utf8>>;

        <<"hlp"/utf8>> ->
            <<"application/winhlp"/utf8>>;

        <<"hpgl"/utf8>> ->
            <<"application/vnd.hp-hpgl"/utf8>>;

        <<"hpid"/utf8>> ->
            <<"application/vnd.hp-hpid"/utf8>>;

        <<"hps"/utf8>> ->
            <<"application/vnd.hp-hps"/utf8>>;

        <<"hqx"/utf8>> ->
            <<"application/mac-binhex40"/utf8>>;

        <<"htke"/utf8>> ->
            <<"application/vnd.kenameaapp"/utf8>>;

        <<"htm"/utf8>> ->
            <<"text/html"/utf8>>;

        <<"html"/utf8>> ->
            <<"text/html"/utf8>>;

        <<"hvd"/utf8>> ->
            <<"application/vnd.yamaha.hv-dic"/utf8>>;

        <<"hvp"/utf8>> ->
            <<"application/vnd.yamaha.hv-voice"/utf8>>;

        <<"hvs"/utf8>> ->
            <<"application/vnd.yamaha.hv-script"/utf8>>;

        <<"i2g"/utf8>> ->
            <<"application/vnd.intergeo"/utf8>>;

        <<"icc"/utf8>> ->
            <<"application/vnd.iccprofile"/utf8>>;

        <<"ice"/utf8>> ->
            <<"x-conference/x-cooltalk"/utf8>>;

        <<"icm"/utf8>> ->
            <<"application/vnd.iccprofile"/utf8>>;

        <<"ico"/utf8>> ->
            <<"image/x-icon"/utf8>>;

        <<"ics"/utf8>> ->
            <<"text/calendar"/utf8>>;

        <<"ief"/utf8>> ->
            <<"image/ief"/utf8>>;

        <<"ifb"/utf8>> ->
            <<"text/calendar"/utf8>>;

        <<"ifm"/utf8>> ->
            <<"application/vnd.shana.informed.formdata"/utf8>>;

        <<"iges"/utf8>> ->
            <<"model/iges"/utf8>>;

        <<"igl"/utf8>> ->
            <<"application/vnd.igloader"/utf8>>;

        <<"igm"/utf8>> ->
            <<"application/vnd.insors.igm"/utf8>>;

        <<"igs"/utf8>> ->
            <<"model/iges"/utf8>>;

        <<"igx"/utf8>> ->
            <<"application/vnd.micrografx.igx"/utf8>>;

        <<"iif"/utf8>> ->
            <<"application/vnd.shana.informed.interchange"/utf8>>;

        <<"imp"/utf8>> ->
            <<"application/vnd.accpac.simply.imp"/utf8>>;

        <<"ims"/utf8>> ->
            <<"application/vnd.ms-ims"/utf8>>;

        <<"in"/utf8>> ->
            <<"text/plain"/utf8>>;

        <<"ink"/utf8>> ->
            <<"application/inkml+xml"/utf8>>;

        <<"inkml"/utf8>> ->
            <<"application/inkml+xml"/utf8>>;

        <<"install"/utf8>> ->
            <<"application/x-install-instructions"/utf8>>;

        <<"iota"/utf8>> ->
            <<"application/vnd.astraea-software.iota"/utf8>>;

        <<"ipfix"/utf8>> ->
            <<"application/ipfix"/utf8>>;

        <<"ipk"/utf8>> ->
            <<"application/vnd.shana.informed.package"/utf8>>;

        <<"irm"/utf8>> ->
            <<"application/vnd.ibm.rights-management"/utf8>>;

        <<"irp"/utf8>> ->
            <<"application/vnd.irepository.package+xml"/utf8>>;

        <<"iso"/utf8>> ->
            <<"application/x-iso9660-image"/utf8>>;

        <<"itp"/utf8>> ->
            <<"application/vnd.shana.informed.formtemplate"/utf8>>;

        <<"ivp"/utf8>> ->
            <<"application/vnd.immervision-ivp"/utf8>>;

        <<"ivu"/utf8>> ->
            <<"application/vnd.immervision-ivu"/utf8>>;

        <<"jad"/utf8>> ->
            <<"text/vnd.sun.j2me.app-descriptor"/utf8>>;

        <<"jam"/utf8>> ->
            <<"application/vnd.jam"/utf8>>;

        <<"jar"/utf8>> ->
            <<"application/java-archive"/utf8>>;

        <<"java"/utf8>> ->
            <<"text/x-java-source"/utf8>>;

        <<"jisp"/utf8>> ->
            <<"application/vnd.jisp"/utf8>>;

        <<"jlt"/utf8>> ->
            <<"application/vnd.hp-jlyt"/utf8>>;

        <<"jnlp"/utf8>> ->
            <<"application/x-java-jnlp-file"/utf8>>;

        <<"joda"/utf8>> ->
            <<"application/vnd.joost.joda-archive"/utf8>>;

        <<"jpe"/utf8>> ->
            <<"image/jpeg"/utf8>>;

        <<"jpeg"/utf8>> ->
            <<"image/jpeg"/utf8>>;

        <<"jpg"/utf8>> ->
            <<"image/jpeg"/utf8>>;

        <<"jpgm"/utf8>> ->
            <<"video/jpm"/utf8>>;

        <<"jpgv"/utf8>> ->
            <<"video/jpeg"/utf8>>;

        <<"jpm"/utf8>> ->
            <<"video/jpm"/utf8>>;

        <<"js"/utf8>> ->
            <<"text/javascript"/utf8>>;

        <<"json"/utf8>> ->
            <<"application/json"/utf8>>;

        <<"jsonml"/utf8>> ->
            <<"application/jsonml+json"/utf8>>;

        <<"jxl"/utf8>> ->
            <<"image/jxl"/utf8>>;

        <<"kar"/utf8>> ->
            <<"audio/midi"/utf8>>;

        <<"karbon"/utf8>> ->
            <<"application/vnd.kde.karbon"/utf8>>;

        <<"kfo"/utf8>> ->
            <<"application/vnd.kde.kformula"/utf8>>;

        <<"kia"/utf8>> ->
            <<"application/vnd.kidspiration"/utf8>>;

        <<"kml"/utf8>> ->
            <<"application/vnd.google-earth.kml+xml"/utf8>>;

        <<"kmz"/utf8>> ->
            <<"application/vnd.google-earth.kmz"/utf8>>;

        <<"kne"/utf8>> ->
            <<"application/vnd.kinar"/utf8>>;

        <<"knp"/utf8>> ->
            <<"application/vnd.kinar"/utf8>>;

        <<"kon"/utf8>> ->
            <<"application/vnd.kde.kontour"/utf8>>;

        <<"kpr"/utf8>> ->
            <<"application/vnd.kde.kpresenter"/utf8>>;

        <<"kpt"/utf8>> ->
            <<"application/vnd.kde.kpresenter"/utf8>>;

        <<"kpxx"/utf8>> ->
            <<"application/vnd.ds-keypoint"/utf8>>;

        <<"ksp"/utf8>> ->
            <<"application/vnd.kde.kspread"/utf8>>;

        <<"ktr"/utf8>> ->
            <<"application/vnd.kahootz"/utf8>>;

        <<"ktx"/utf8>> ->
            <<"image/ktx"/utf8>>;

        <<"ktz"/utf8>> ->
            <<"application/vnd.kahootz"/utf8>>;

        <<"kwd"/utf8>> ->
            <<"application/vnd.kde.kword"/utf8>>;

        <<"kwt"/utf8>> ->
            <<"application/vnd.kde.kword"/utf8>>;

        <<"lasxml"/utf8>> ->
            <<"application/vnd.las.las+xml"/utf8>>;

        <<"latex"/utf8>> ->
            <<"application/x-latex"/utf8>>;

        <<"lbd"/utf8>> ->
            <<"application/vnd.llamagraphics.life-balance.desktop"/utf8>>;

        <<"lbe"/utf8>> ->
            <<"application/vnd.llamagraphics.life-balance.exchange+xml"/utf8>>;

        <<"les"/utf8>> ->
            <<"application/vnd.hhe.lesson-player"/utf8>>;

        <<"lha"/utf8>> ->
            <<"application/x-lzh-compressed"/utf8>>;

        <<"link66"/utf8>> ->
            <<"application/vnd.route66.link66+xml"/utf8>>;

        <<"list"/utf8>> ->
            <<"text/plain"/utf8>>;

        <<"list3820"/utf8>> ->
            <<"application/vnd.ibm.modcap"/utf8>>;

        <<"listafp"/utf8>> ->
            <<"application/vnd.ibm.modcap"/utf8>>;

        <<"lnk"/utf8>> ->
            <<"application/x-ms-shortcut"/utf8>>;

        <<"log"/utf8>> ->
            <<"text/plain"/utf8>>;

        <<"lostxml"/utf8>> ->
            <<"application/lost+xml"/utf8>>;

        <<"lrf"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"lrm"/utf8>> ->
            <<"application/vnd.ms-lrm"/utf8>>;

        <<"ltf"/utf8>> ->
            <<"application/vnd.frogans.ltf"/utf8>>;

        <<"lvp"/utf8>> ->
            <<"audio/vnd.lucent.voice"/utf8>>;

        <<"lwp"/utf8>> ->
            <<"application/vnd.lotus-wordpro"/utf8>>;

        <<"lzh"/utf8>> ->
            <<"application/x-lzh-compressed"/utf8>>;

        <<"m13"/utf8>> ->
            <<"application/x-msmediaview"/utf8>>;

        <<"m14"/utf8>> ->
            <<"application/x-msmediaview"/utf8>>;

        <<"m1v"/utf8>> ->
            <<"video/mpeg"/utf8>>;

        <<"m21"/utf8>> ->
            <<"application/mp21"/utf8>>;

        <<"m2a"/utf8>> ->
            <<"audio/mpeg"/utf8>>;

        <<"m2t"/utf8>> ->
            <<"video/mp2t"/utf8>>;

        <<"m2ts"/utf8>> ->
            <<"video/mp2t"/utf8>>;

        <<"m2v"/utf8>> ->
            <<"video/mpeg"/utf8>>;

        <<"m3a"/utf8>> ->
            <<"audio/mpeg"/utf8>>;

        <<"m3u"/utf8>> ->
            <<"audio/x-mpegurl"/utf8>>;

        <<"m3u8"/utf8>> ->
            <<"application/vnd.apple.mpegurl"/utf8>>;

        <<"m4a"/utf8>> ->
            <<"audio/mp4"/utf8>>;

        <<"m4u"/utf8>> ->
            <<"video/vnd.mpegurl"/utf8>>;

        <<"m4v"/utf8>> ->
            <<"video/x-m4v"/utf8>>;

        <<"ma"/utf8>> ->
            <<"application/mathematica"/utf8>>;

        <<"mads"/utf8>> ->
            <<"application/mads+xml"/utf8>>;

        <<"mag"/utf8>> ->
            <<"application/vnd.ecowin.chart"/utf8>>;

        <<"maker"/utf8>> ->
            <<"application/vnd.framemaker"/utf8>>;

        <<"man"/utf8>> ->
            <<"text/troff"/utf8>>;

        <<"mar"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"mathml"/utf8>> ->
            <<"application/mathml+xml"/utf8>>;

        <<"mb"/utf8>> ->
            <<"application/mathematica"/utf8>>;

        <<"mbk"/utf8>> ->
            <<"application/vnd.mobius.mbk"/utf8>>;

        <<"mbox"/utf8>> ->
            <<"application/mbox"/utf8>>;

        <<"mc1"/utf8>> ->
            <<"application/vnd.medcalcdata"/utf8>>;

        <<"mcd"/utf8>> ->
            <<"application/vnd.mcd"/utf8>>;

        <<"mcurl"/utf8>> ->
            <<"text/vnd.curl.mcurl"/utf8>>;

        <<"mdb"/utf8>> ->
            <<"application/x-msaccess"/utf8>>;

        <<"mdi"/utf8>> ->
            <<"image/vnd.ms-modi"/utf8>>;

        <<"me"/utf8>> ->
            <<"text/troff"/utf8>>;

        <<"mesh"/utf8>> ->
            <<"model/mesh"/utf8>>;

        <<"meta4"/utf8>> ->
            <<"application/metalink4+xml"/utf8>>;

        <<"metalink"/utf8>> ->
            <<"application/metalink+xml"/utf8>>;

        <<"mets"/utf8>> ->
            <<"application/mets+xml"/utf8>>;

        <<"mfm"/utf8>> ->
            <<"application/vnd.mfmp"/utf8>>;

        <<"mft"/utf8>> ->
            <<"application/rpki-manifest"/utf8>>;

        <<"mgp"/utf8>> ->
            <<"application/vnd.osgeo.mapguide.package"/utf8>>;

        <<"mgz"/utf8>> ->
            <<"application/vnd.proteus.magazine"/utf8>>;

        <<"mid"/utf8>> ->
            <<"audio/midi"/utf8>>;

        <<"midi"/utf8>> ->
            <<"audio/midi"/utf8>>;

        <<"mie"/utf8>> ->
            <<"application/x-mie"/utf8>>;

        <<"mif"/utf8>> ->
            <<"application/vnd.mif"/utf8>>;

        <<"mime"/utf8>> ->
            <<"message/rfc822"/utf8>>;

        <<"mj2"/utf8>> ->
            <<"video/mj2"/utf8>>;

        <<"mjp2"/utf8>> ->
            <<"video/mj2"/utf8>>;

        <<"mjs"/utf8>> ->
            <<"text/javascript"/utf8>>;

        <<"mk3d"/utf8>> ->
            <<"video/x-matroska"/utf8>>;

        <<"mka"/utf8>> ->
            <<"audio/x-matroska"/utf8>>;

        <<"mks"/utf8>> ->
            <<"video/x-matroska"/utf8>>;

        <<"mkv"/utf8>> ->
            <<"video/x-matroska"/utf8>>;

        <<"mlp"/utf8>> ->
            <<"application/vnd.dolby.mlp"/utf8>>;

        <<"mmd"/utf8>> ->
            <<"application/vnd.chipnuts.karaoke-mmd"/utf8>>;

        <<"mmf"/utf8>> ->
            <<"application/vnd.smaf"/utf8>>;

        <<"mmr"/utf8>> ->
            <<"image/vnd.fujixerox.edmics-mmr"/utf8>>;

        <<"mng"/utf8>> ->
            <<"video/x-mng"/utf8>>;

        <<"mny"/utf8>> ->
            <<"application/x-msmoney"/utf8>>;

        <<"mobi"/utf8>> ->
            <<"application/x-mobipocket-ebook"/utf8>>;

        <<"mods"/utf8>> ->
            <<"application/mods+xml"/utf8>>;

        <<"mov"/utf8>> ->
            <<"video/quicktime"/utf8>>;

        <<"movie"/utf8>> ->
            <<"video/x-sgi-movie"/utf8>>;

        <<"mp2"/utf8>> ->
            <<"audio/mpeg"/utf8>>;

        <<"mp21"/utf8>> ->
            <<"application/mp21"/utf8>>;

        <<"mp2a"/utf8>> ->
            <<"audio/mpeg"/utf8>>;

        <<"mp3"/utf8>> ->
            <<"audio/mpeg"/utf8>>;

        <<"mp4"/utf8>> ->
            <<"video/mp4"/utf8>>;

        <<"mp4a"/utf8>> ->
            <<"audio/mp4"/utf8>>;

        <<"mp4s"/utf8>> ->
            <<"application/mp4"/utf8>>;

        <<"mp4v"/utf8>> ->
            <<"video/mp4"/utf8>>;

        <<"mpc"/utf8>> ->
            <<"application/vnd.mophun.certificate"/utf8>>;

        <<"mpe"/utf8>> ->
            <<"video/mpeg"/utf8>>;

        <<"mpeg"/utf8>> ->
            <<"video/mpeg"/utf8>>;

        <<"mpg"/utf8>> ->
            <<"video/mpeg"/utf8>>;

        <<"mpg4"/utf8>> ->
            <<"video/mp4"/utf8>>;

        <<"mpga"/utf8>> ->
            <<"audio/mpeg"/utf8>>;

        <<"mpkg"/utf8>> ->
            <<"application/vnd.apple.installer+xml"/utf8>>;

        <<"mpm"/utf8>> ->
            <<"application/vnd.blueice.multipass"/utf8>>;

        <<"mpn"/utf8>> ->
            <<"application/vnd.mophun.application"/utf8>>;

        <<"mpp"/utf8>> ->
            <<"application/vnd.ms-project"/utf8>>;

        <<"mpt"/utf8>> ->
            <<"application/vnd.ms-project"/utf8>>;

        <<"mpy"/utf8>> ->
            <<"application/vnd.ibm.minipay"/utf8>>;

        <<"mqy"/utf8>> ->
            <<"application/vnd.mobius.mqy"/utf8>>;

        <<"mrc"/utf8>> ->
            <<"application/marc"/utf8>>;

        <<"mrcx"/utf8>> ->
            <<"application/marcxml+xml"/utf8>>;

        <<"ms"/utf8>> ->
            <<"text/troff"/utf8>>;

        <<"mscml"/utf8>> ->
            <<"application/mediaservercontrol+xml"/utf8>>;

        <<"mseed"/utf8>> ->
            <<"application/vnd.fdsn.mseed"/utf8>>;

        <<"mseq"/utf8>> ->
            <<"application/vnd.mseq"/utf8>>;

        <<"msf"/utf8>> ->
            <<"application/vnd.epson.msf"/utf8>>;

        <<"msh"/utf8>> ->
            <<"model/mesh"/utf8>>;

        <<"msi"/utf8>> ->
            <<"application/x-msdownload"/utf8>>;

        <<"msl"/utf8>> ->
            <<"application/vnd.mobius.msl"/utf8>>;

        <<"msty"/utf8>> ->
            <<"application/vnd.muvee.style"/utf8>>;

        <<"mts"/utf8>> ->
            <<"video/mp2t"/utf8>>;

        <<"mus"/utf8>> ->
            <<"application/vnd.musician"/utf8>>;

        <<"musicxml"/utf8>> ->
            <<"application/vnd.recordare.musicxml+xml"/utf8>>;

        <<"mvb"/utf8>> ->
            <<"application/x-msmediaview"/utf8>>;

        <<"mwf"/utf8>> ->
            <<"application/vnd.mfer"/utf8>>;

        <<"mxf"/utf8>> ->
            <<"application/mxf"/utf8>>;

        <<"mxl"/utf8>> ->
            <<"application/vnd.recordare.musicxml"/utf8>>;

        <<"mxml"/utf8>> ->
            <<"application/xv+xml"/utf8>>;

        <<"mxs"/utf8>> ->
            <<"application/vnd.triscape.mxs"/utf8>>;

        <<"mxu"/utf8>> ->
            <<"video/vnd.mpegurl"/utf8>>;

        <<"n-gage"/utf8>> ->
            <<"application/vnd.nokia.n-gage.symbian.install"/utf8>>;

        <<"n3"/utf8>> ->
            <<"text/n3"/utf8>>;

        <<"nb"/utf8>> ->
            <<"application/mathematica"/utf8>>;

        <<"nbp"/utf8>> ->
            <<"application/vnd.wolfram.player"/utf8>>;

        <<"nc"/utf8>> ->
            <<"application/x-netcdf"/utf8>>;

        <<"ncx"/utf8>> ->
            <<"application/x-dtbncx+xml"/utf8>>;

        <<"nfo"/utf8>> ->
            <<"text/x-nfo"/utf8>>;

        <<"ngdat"/utf8>> ->
            <<"application/vnd.nokia.n-gage.data"/utf8>>;

        <<"nitf"/utf8>> ->
            <<"application/vnd.nitf"/utf8>>;

        <<"nlu"/utf8>> ->
            <<"application/vnd.neurolanguage.nlu"/utf8>>;

        <<"nml"/utf8>> ->
            <<"application/vnd.enliven"/utf8>>;

        <<"nnd"/utf8>> ->
            <<"application/vnd.noblenet-directory"/utf8>>;

        <<"nns"/utf8>> ->
            <<"application/vnd.noblenet-sealer"/utf8>>;

        <<"nnw"/utf8>> ->
            <<"application/vnd.noblenet-web"/utf8>>;

        <<"npx"/utf8>> ->
            <<"image/vnd.net-fpx"/utf8>>;

        <<"nsc"/utf8>> ->
            <<"application/x-conference"/utf8>>;

        <<"nsf"/utf8>> ->
            <<"application/vnd.lotus-notes"/utf8>>;

        <<"ntf"/utf8>> ->
            <<"application/vnd.nitf"/utf8>>;

        <<"nzb"/utf8>> ->
            <<"application/x-nzb"/utf8>>;

        <<"oa2"/utf8>> ->
            <<"application/vnd.fujitsu.oasys2"/utf8>>;

        <<"oa3"/utf8>> ->
            <<"application/vnd.fujitsu.oasys3"/utf8>>;

        <<"oas"/utf8>> ->
            <<"application/vnd.fujitsu.oasys"/utf8>>;

        <<"obd"/utf8>> ->
            <<"application/x-msbinder"/utf8>>;

        <<"obj"/utf8>> ->
            <<"application/x-tgif"/utf8>>;

        <<"oda"/utf8>> ->
            <<"application/oda"/utf8>>;

        <<"odb"/utf8>> ->
            <<"application/vnd.oasis.opendocument.database"/utf8>>;

        <<"odc"/utf8>> ->
            <<"application/vnd.oasis.opendocument.chart"/utf8>>;

        <<"odf"/utf8>> ->
            <<"application/vnd.oasis.opendocument.formula"/utf8>>;

        <<"odft"/utf8>> ->
            <<"application/vnd.oasis.opendocument.formula-template"/utf8>>;

        <<"odg"/utf8>> ->
            <<"application/vnd.oasis.opendocument.graphics"/utf8>>;

        <<"odi"/utf8>> ->
            <<"application/vnd.oasis.opendocument.image"/utf8>>;

        <<"odm"/utf8>> ->
            <<"application/vnd.oasis.opendocument.text-master"/utf8>>;

        <<"odp"/utf8>> ->
            <<"application/vnd.oasis.opendocument.presentation"/utf8>>;

        <<"ods"/utf8>> ->
            <<"application/vnd.oasis.opendocument.spreadsheet"/utf8>>;

        <<"odt"/utf8>> ->
            <<"application/vnd.oasis.opendocument.text"/utf8>>;

        <<"oga"/utf8>> ->
            <<"audio/ogg"/utf8>>;

        <<"ogg"/utf8>> ->
            <<"audio/ogg"/utf8>>;

        <<"ogv"/utf8>> ->
            <<"video/ogg"/utf8>>;

        <<"ogx"/utf8>> ->
            <<"application/ogg"/utf8>>;

        <<"omdoc"/utf8>> ->
            <<"application/omdoc+xml"/utf8>>;

        <<"onepkg"/utf8>> ->
            <<"application/onenote"/utf8>>;

        <<"onetmp"/utf8>> ->
            <<"application/onenote"/utf8>>;

        <<"onetoc"/utf8>> ->
            <<"application/onenote"/utf8>>;

        <<"onetoc2"/utf8>> ->
            <<"application/onenote"/utf8>>;

        <<"opf"/utf8>> ->
            <<"application/oebps-package+xml"/utf8>>;

        <<"opml"/utf8>> ->
            <<"text/x-opml"/utf8>>;

        <<"oprc"/utf8>> ->
            <<"application/vnd.palm"/utf8>>;

        <<"opus"/utf8>> ->
            <<"audio/ogg"/utf8>>;

        <<"org"/utf8>> ->
            <<"application/vnd.lotus-organizer"/utf8>>;

        <<"osf"/utf8>> ->
            <<"application/vnd.yamaha.openscoreformat"/utf8>>;

        <<"osfpvg"/utf8>> ->
            <<"application/vnd.yamaha.openscoreformat.osfpvg+xml"/utf8>>;

        <<"otc"/utf8>> ->
            <<"application/vnd.oasis.opendocument.chart-template"/utf8>>;

        <<"otf"/utf8>> ->
            <<"font/otf"/utf8>>;

        <<"otg"/utf8>> ->
            <<"application/vnd.oasis.opendocument.graphics-template"/utf8>>;

        <<"oth"/utf8>> ->
            <<"application/vnd.oasis.opendocument.text-web"/utf8>>;

        <<"oti"/utf8>> ->
            <<"application/vnd.oasis.opendocument.image-template"/utf8>>;

        <<"otp"/utf8>> ->
            <<"application/vnd.oasis.opendocument.presentation-template"/utf8>>;

        <<"ots"/utf8>> ->
            <<"application/vnd.oasis.opendocument.spreadsheet-template"/utf8>>;

        <<"ott"/utf8>> ->
            <<"application/vnd.oasis.opendocument.text-template"/utf8>>;

        <<"oxps"/utf8>> ->
            <<"application/oxps"/utf8>>;

        <<"oxt"/utf8>> ->
            <<"application/vnd.openofficeorg.extension"/utf8>>;

        <<"p"/utf8>> ->
            <<"text/x-pascal"/utf8>>;

        <<"p10"/utf8>> ->
            <<"application/pkcs10"/utf8>>;

        <<"p12"/utf8>> ->
            <<"application/x-pkcs12"/utf8>>;

        <<"p7b"/utf8>> ->
            <<"application/x-pkcs7-certificates"/utf8>>;

        <<"p7c"/utf8>> ->
            <<"application/pkcs7-mime"/utf8>>;

        <<"p7m"/utf8>> ->
            <<"application/pkcs7-mime"/utf8>>;

        <<"p7r"/utf8>> ->
            <<"application/x-pkcs7-certreqresp"/utf8>>;

        <<"p7s"/utf8>> ->
            <<"application/pkcs7-signature"/utf8>>;

        <<"p8"/utf8>> ->
            <<"application/pkcs8"/utf8>>;

        <<"pas"/utf8>> ->
            <<"text/x-pascal"/utf8>>;

        <<"paw"/utf8>> ->
            <<"application/vnd.pawaafile"/utf8>>;

        <<"pbd"/utf8>> ->
            <<"application/vnd.powerbuilder6"/utf8>>;

        <<"pbm"/utf8>> ->
            <<"image/x-portable-bitmap"/utf8>>;

        <<"pcap"/utf8>> ->
            <<"application/vnd.tcpdump.pcap"/utf8>>;

        <<"pcf"/utf8>> ->
            <<"application/x-font-pcf"/utf8>>;

        <<"pcl"/utf8>> ->
            <<"application/vnd.hp-pcl"/utf8>>;

        <<"pclxl"/utf8>> ->
            <<"application/vnd.hp-pclxl"/utf8>>;

        <<"pct"/utf8>> ->
            <<"image/x-pict"/utf8>>;

        <<"pcurl"/utf8>> ->
            <<"application/vnd.curl.pcurl"/utf8>>;

        <<"pcx"/utf8>> ->
            <<"image/x-pcx"/utf8>>;

        <<"pdb"/utf8>> ->
            <<"application/vnd.palm"/utf8>>;

        <<"pdf"/utf8>> ->
            <<"application/pdf"/utf8>>;

        <<"pfa"/utf8>> ->
            <<"application/x-font-type1"/utf8>>;

        <<"pfb"/utf8>> ->
            <<"application/x-font-type1"/utf8>>;

        <<"pfm"/utf8>> ->
            <<"application/x-font-type1"/utf8>>;

        <<"pfr"/utf8>> ->
            <<"application/font-tdpfr"/utf8>>;

        <<"pfx"/utf8>> ->
            <<"application/x-pkcs12"/utf8>>;

        <<"pgm"/utf8>> ->
            <<"image/x-portable-graymap"/utf8>>;

        <<"pgn"/utf8>> ->
            <<"application/x-chess-pgn"/utf8>>;

        <<"pgp"/utf8>> ->
            <<"application/pgp-encrypted"/utf8>>;

        <<"pic"/utf8>> ->
            <<"image/x-pict"/utf8>>;

        <<"pkg"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"pki"/utf8>> ->
            <<"application/pkixcmp"/utf8>>;

        <<"pkipath"/utf8>> ->
            <<"application/pkix-pkipath"/utf8>>;

        <<"plb"/utf8>> ->
            <<"application/vnd.3gpp.pic-bw-large"/utf8>>;

        <<"plc"/utf8>> ->
            <<"application/vnd.mobius.plc"/utf8>>;

        <<"plf"/utf8>> ->
            <<"application/vnd.pocketlearn"/utf8>>;

        <<"pls"/utf8>> ->
            <<"application/pls+xml"/utf8>>;

        <<"pml"/utf8>> ->
            <<"application/vnd.ctc-posml"/utf8>>;

        <<"png"/utf8>> ->
            <<"image/png"/utf8>>;

        <<"pnm"/utf8>> ->
            <<"image/x-portable-anymap"/utf8>>;

        <<"portpkg"/utf8>> ->
            <<"application/vnd.macports.portpkg"/utf8>>;

        <<"pot"/utf8>> ->
            <<"application/vnd.ms-powerpoint"/utf8>>;

        <<"potm"/utf8>> ->
            <<"application/vnd.ms-powerpoint.template.macroenabled.12"/utf8>>;

        <<"potx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.presentationml.template"/utf8>>;

        <<"ppam"/utf8>> ->
            <<"application/vnd.ms-powerpoint.addin.macroenabled.12"/utf8>>;

        <<"ppd"/utf8>> ->
            <<"application/vnd.cups-ppd"/utf8>>;

        <<"ppm"/utf8>> ->
            <<"image/x-portable-pixmap"/utf8>>;

        <<"pps"/utf8>> ->
            <<"application/vnd.ms-powerpoint"/utf8>>;

        <<"ppsm"/utf8>> ->
            <<"application/vnd.ms-powerpoint.slideshow.macroenabled.12"/utf8>>;

        <<"ppsx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.presentationml.slideshow"/utf8>>;

        <<"ppt"/utf8>> ->
            <<"application/vnd.ms-powerpoint"/utf8>>;

        <<"pptm"/utf8>> ->
            <<"application/vnd.ms-powerpoint.presentation.macroenabled.12"/utf8>>;

        <<"pptx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.presentationml.presentation"/utf8>>;

        <<"pqa"/utf8>> ->
            <<"application/vnd.palm"/utf8>>;

        <<"prc"/utf8>> ->
            <<"application/x-mobipocket-ebook"/utf8>>;

        <<"pre"/utf8>> ->
            <<"application/vnd.lotus-freelance"/utf8>>;

        <<"prf"/utf8>> ->
            <<"application/pics-rules"/utf8>>;

        <<"ps"/utf8>> ->
            <<"application/postscript"/utf8>>;

        <<"psb"/utf8>> ->
            <<"application/vnd.3gpp.pic-bw-small"/utf8>>;

        <<"psd"/utf8>> ->
            <<"image/vnd.adobe.photoshop"/utf8>>;

        <<"psf"/utf8>> ->
            <<"application/x-font-linux-psf"/utf8>>;

        <<"pskcxml"/utf8>> ->
            <<"application/pskc+xml"/utf8>>;

        <<"ptid"/utf8>> ->
            <<"application/vnd.pvi.ptid1"/utf8>>;

        <<"pub"/utf8>> ->
            <<"application/x-mspublisher"/utf8>>;

        <<"pvb"/utf8>> ->
            <<"application/vnd.3gpp.pic-bw-var"/utf8>>;

        <<"pwn"/utf8>> ->
            <<"application/vnd.3m.post-it-notes"/utf8>>;

        <<"pya"/utf8>> ->
            <<"audio/vnd.ms-playready.media.pya"/utf8>>;

        <<"pyv"/utf8>> ->
            <<"video/vnd.ms-playready.media.pyv"/utf8>>;

        <<"qam"/utf8>> ->
            <<"application/vnd.epson.quickanime"/utf8>>;

        <<"qbo"/utf8>> ->
            <<"application/vnd.intu.qbo"/utf8>>;

        <<"qfx"/utf8>> ->
            <<"application/vnd.intu.qfx"/utf8>>;

        <<"qps"/utf8>> ->
            <<"application/vnd.publishare-delta-tree"/utf8>>;

        <<"qt"/utf8>> ->
            <<"video/quicktime"/utf8>>;

        <<"qwd"/utf8>> ->
            <<"application/vnd.quark.quarkxpress"/utf8>>;

        <<"qwt"/utf8>> ->
            <<"application/vnd.quark.quarkxpress"/utf8>>;

        <<"qxb"/utf8>> ->
            <<"application/vnd.quark.quarkxpress"/utf8>>;

        <<"qxd"/utf8>> ->
            <<"application/vnd.quark.quarkxpress"/utf8>>;

        <<"qxl"/utf8>> ->
            <<"application/vnd.quark.quarkxpress"/utf8>>;

        <<"qxt"/utf8>> ->
            <<"application/vnd.quark.quarkxpress"/utf8>>;

        <<"ra"/utf8>> ->
            <<"audio/x-pn-realaudio"/utf8>>;

        <<"ram"/utf8>> ->
            <<"audio/x-pn-realaudio"/utf8>>;

        <<"rar"/utf8>> ->
            <<"application/x-rar-compressed"/utf8>>;

        <<"ras"/utf8>> ->
            <<"image/x-cmu-raster"/utf8>>;

        <<"rcprofile"/utf8>> ->
            <<"application/vnd.ipunplugged.rcprofile"/utf8>>;

        <<"rdf"/utf8>> ->
            <<"application/rdf+xml"/utf8>>;

        <<"rdz"/utf8>> ->
            <<"application/vnd.data-vision.rdz"/utf8>>;

        <<"rep"/utf8>> ->
            <<"application/vnd.businessobjects"/utf8>>;

        <<"res"/utf8>> ->
            <<"application/x-dtbresource+xml"/utf8>>;

        <<"rgb"/utf8>> ->
            <<"image/x-rgb"/utf8>>;

        <<"rif"/utf8>> ->
            <<"application/reginfo+xml"/utf8>>;

        <<"rip"/utf8>> ->
            <<"audio/vnd.rip"/utf8>>;

        <<"ris"/utf8>> ->
            <<"application/x-research-info-systems"/utf8>>;

        <<"rl"/utf8>> ->
            <<"application/resource-lists+xml"/utf8>>;

        <<"rlc"/utf8>> ->
            <<"image/vnd.fujixerox.edmics-rlc"/utf8>>;

        <<"rld"/utf8>> ->
            <<"application/resource-lists-diff+xml"/utf8>>;

        <<"rm"/utf8>> ->
            <<"application/vnd.rn-realmedia"/utf8>>;

        <<"rmi"/utf8>> ->
            <<"audio/midi"/utf8>>;

        <<"rmp"/utf8>> ->
            <<"audio/x-pn-realaudio-plugin"/utf8>>;

        <<"rms"/utf8>> ->
            <<"application/vnd.jcp.javame.midlet-rms"/utf8>>;

        <<"rmvb"/utf8>> ->
            <<"application/vnd.rn-realmedia-vbr"/utf8>>;

        <<"rnc"/utf8>> ->
            <<"application/relax-ng-compact-syntax"/utf8>>;

        <<"roa"/utf8>> ->
            <<"application/rpki-roa"/utf8>>;

        <<"roff"/utf8>> ->
            <<"text/troff"/utf8>>;

        <<"rp9"/utf8>> ->
            <<"application/vnd.cloanto.rp9"/utf8>>;

        <<"rpss"/utf8>> ->
            <<"application/vnd.nokia.radio-presets"/utf8>>;

        <<"rpst"/utf8>> ->
            <<"application/vnd.nokia.radio-preset"/utf8>>;

        <<"rq"/utf8>> ->
            <<"application/sparql-query"/utf8>>;

        <<"rs"/utf8>> ->
            <<"application/rls-services+xml"/utf8>>;

        <<"rsd"/utf8>> ->
            <<"application/rsd+xml"/utf8>>;

        <<"rss"/utf8>> ->
            <<"application/rss+xml"/utf8>>;

        <<"rtf"/utf8>> ->
            <<"application/rtf"/utf8>>;

        <<"rtx"/utf8>> ->
            <<"text/richtext"/utf8>>;

        <<"s"/utf8>> ->
            <<"text/x-asm"/utf8>>;

        <<"s3m"/utf8>> ->
            <<"audio/s3m"/utf8>>;

        <<"saf"/utf8>> ->
            <<"application/vnd.yamaha.smaf-audio"/utf8>>;

        <<"sbml"/utf8>> ->
            <<"application/sbml+xml"/utf8>>;

        <<"sc"/utf8>> ->
            <<"application/vnd.ibm.secure-container"/utf8>>;

        <<"scd"/utf8>> ->
            <<"application/x-msschedule"/utf8>>;

        <<"scm"/utf8>> ->
            <<"application/vnd.lotus-screencam"/utf8>>;

        <<"scq"/utf8>> ->
            <<"application/scvp-cv-request"/utf8>>;

        <<"scs"/utf8>> ->
            <<"application/scvp-cv-response"/utf8>>;

        <<"scurl"/utf8>> ->
            <<"text/vnd.curl.scurl"/utf8>>;

        <<"sda"/utf8>> ->
            <<"application/vnd.stardivision.draw"/utf8>>;

        <<"sdc"/utf8>> ->
            <<"application/vnd.stardivision.calc"/utf8>>;

        <<"sdd"/utf8>> ->
            <<"application/vnd.stardivision.impress"/utf8>>;

        <<"sdkd"/utf8>> ->
            <<"application/vnd.solent.sdkm+xml"/utf8>>;

        <<"sdkm"/utf8>> ->
            <<"application/vnd.solent.sdkm+xml"/utf8>>;

        <<"sdp"/utf8>> ->
            <<"application/sdp"/utf8>>;

        <<"sdw"/utf8>> ->
            <<"application/vnd.stardivision.writer"/utf8>>;

        <<"see"/utf8>> ->
            <<"application/vnd.seemail"/utf8>>;

        <<"seed"/utf8>> ->
            <<"application/vnd.fdsn.seed"/utf8>>;

        <<"sema"/utf8>> ->
            <<"application/vnd.sema"/utf8>>;

        <<"semd"/utf8>> ->
            <<"application/vnd.semd"/utf8>>;

        <<"semf"/utf8>> ->
            <<"application/vnd.semf"/utf8>>;

        <<"ser"/utf8>> ->
            <<"application/java-serialized-object"/utf8>>;

        <<"setpay"/utf8>> ->
            <<"application/set-payment-initiation"/utf8>>;

        <<"setreg"/utf8>> ->
            <<"application/set-registration-initiation"/utf8>>;

        <<"sfd-hdstx"/utf8>> ->
            <<"application/vnd.hydrostatix.sof-data"/utf8>>;

        <<"sfs"/utf8>> ->
            <<"application/vnd.spotfire.sfs"/utf8>>;

        <<"sfv"/utf8>> ->
            <<"text/x-sfv"/utf8>>;

        <<"sgi"/utf8>> ->
            <<"image/sgi"/utf8>>;

        <<"sgl"/utf8>> ->
            <<"application/vnd.stardivision.writer-global"/utf8>>;

        <<"sgm"/utf8>> ->
            <<"text/sgml"/utf8>>;

        <<"sgml"/utf8>> ->
            <<"text/sgml"/utf8>>;

        <<"sh"/utf8>> ->
            <<"application/x-sh"/utf8>>;

        <<"shar"/utf8>> ->
            <<"application/x-shar"/utf8>>;

        <<"shf"/utf8>> ->
            <<"application/shf+xml"/utf8>>;

        <<"sid"/utf8>> ->
            <<"image/x-mrsid-image"/utf8>>;

        <<"sig"/utf8>> ->
            <<"application/pgp-signature"/utf8>>;

        <<"sil"/utf8>> ->
            <<"audio/silk"/utf8>>;

        <<"silo"/utf8>> ->
            <<"model/mesh"/utf8>>;

        <<"sis"/utf8>> ->
            <<"application/vnd.symbian.install"/utf8>>;

        <<"sisx"/utf8>> ->
            <<"application/vnd.symbian.install"/utf8>>;

        <<"sit"/utf8>> ->
            <<"application/x-stuffit"/utf8>>;

        <<"sitx"/utf8>> ->
            <<"application/x-stuffitx"/utf8>>;

        <<"skd"/utf8>> ->
            <<"application/vnd.koan"/utf8>>;

        <<"skm"/utf8>> ->
            <<"application/vnd.koan"/utf8>>;

        <<"skp"/utf8>> ->
            <<"application/vnd.koan"/utf8>>;

        <<"skt"/utf8>> ->
            <<"application/vnd.koan"/utf8>>;

        <<"sldm"/utf8>> ->
            <<"application/vnd.ms-powerpoint.slide.macroenabled.12"/utf8>>;

        <<"sldx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.presentationml.slide"/utf8>>;

        <<"slt"/utf8>> ->
            <<"application/vnd.epson.salt"/utf8>>;

        <<"sm"/utf8>> ->
            <<"application/vnd.stepmania.stepchart"/utf8>>;

        <<"smf"/utf8>> ->
            <<"application/vnd.stardivision.math"/utf8>>;

        <<"smi"/utf8>> ->
            <<"application/smil+xml"/utf8>>;

        <<"smil"/utf8>> ->
            <<"application/smil+xml"/utf8>>;

        <<"smv"/utf8>> ->
            <<"video/x-smv"/utf8>>;

        <<"smzip"/utf8>> ->
            <<"application/vnd.stepmania.package"/utf8>>;

        <<"snd"/utf8>> ->
            <<"audio/basic"/utf8>>;

        <<"snf"/utf8>> ->
            <<"application/x-font-snf"/utf8>>;

        <<"so"/utf8>> ->
            <<"application/octet-stream"/utf8>>;

        <<"spc"/utf8>> ->
            <<"application/x-pkcs7-certificates"/utf8>>;

        <<"spf"/utf8>> ->
            <<"application/vnd.yamaha.smaf-phrase"/utf8>>;

        <<"spl"/utf8>> ->
            <<"application/x-futuresplash"/utf8>>;

        <<"spot"/utf8>> ->
            <<"text/vnd.in3d.spot"/utf8>>;

        <<"spp"/utf8>> ->
            <<"application/scvp-vp-response"/utf8>>;

        <<"spq"/utf8>> ->
            <<"application/scvp-vp-request"/utf8>>;

        <<"spx"/utf8>> ->
            <<"audio/ogg"/utf8>>;

        <<"sql"/utf8>> ->
            <<"application/x-sql"/utf8>>;

        <<"src"/utf8>> ->
            <<"application/x-wais-source"/utf8>>;

        <<"srt"/utf8>> ->
            <<"application/x-subrip"/utf8>>;

        <<"sru"/utf8>> ->
            <<"application/sru+xml"/utf8>>;

        <<"srx"/utf8>> ->
            <<"application/sparql-results+xml"/utf8>>;

        <<"ssdl"/utf8>> ->
            <<"application/ssdl+xml"/utf8>>;

        <<"sse"/utf8>> ->
            <<"application/vnd.kodak-descriptor"/utf8>>;

        <<"ssf"/utf8>> ->
            <<"application/vnd.epson.ssf"/utf8>>;

        <<"ssml"/utf8>> ->
            <<"application/ssml+xml"/utf8>>;

        <<"st"/utf8>> ->
            <<"application/vnd.sailingtracker.track"/utf8>>;

        <<"stc"/utf8>> ->
            <<"application/vnd.sun.xml.calc.template"/utf8>>;

        <<"std"/utf8>> ->
            <<"application/vnd.sun.xml.draw.template"/utf8>>;

        <<"stf"/utf8>> ->
            <<"application/vnd.wt.stf"/utf8>>;

        <<"sti"/utf8>> ->
            <<"application/vnd.sun.xml.impress.template"/utf8>>;

        <<"stk"/utf8>> ->
            <<"application/hyperstudio"/utf8>>;

        <<"stl"/utf8>> ->
            <<"application/vnd.ms-pki.stl"/utf8>>;

        <<"str"/utf8>> ->
            <<"application/vnd.pg.format"/utf8>>;

        <<"stw"/utf8>> ->
            <<"application/vnd.sun.xml.writer.template"/utf8>>;

        <<"sub"/utf8>> ->
            <<"image/vnd.dvb.subtitle"/utf8>>;

        <<"sus"/utf8>> ->
            <<"application/vnd.sus-calendar"/utf8>>;

        <<"susp"/utf8>> ->
            <<"application/vnd.sus-calendar"/utf8>>;

        <<"sv4cpio"/utf8>> ->
            <<"application/x-sv4cpio"/utf8>>;

        <<"sv4crc"/utf8>> ->
            <<"application/x-sv4crc"/utf8>>;

        <<"svc"/utf8>> ->
            <<"application/vnd.dvb.service"/utf8>>;

        <<"svd"/utf8>> ->
            <<"application/vnd.svd"/utf8>>;

        <<"svg"/utf8>> ->
            <<"image/svg+xml"/utf8>>;

        <<"svgz"/utf8>> ->
            <<"image/svg+xml"/utf8>>;

        <<"swa"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"swf"/utf8>> ->
            <<"application/x-shockwave-flash"/utf8>>;

        <<"swi"/utf8>> ->
            <<"application/vnd.aristanetworks.swi"/utf8>>;

        <<"sxc"/utf8>> ->
            <<"application/vnd.sun.xml.calc"/utf8>>;

        <<"sxd"/utf8>> ->
            <<"application/vnd.sun.xml.draw"/utf8>>;

        <<"sxg"/utf8>> ->
            <<"application/vnd.sun.xml.writer.global"/utf8>>;

        <<"sxi"/utf8>> ->
            <<"application/vnd.sun.xml.impress"/utf8>>;

        <<"sxm"/utf8>> ->
            <<"application/vnd.sun.xml.math"/utf8>>;

        <<"sxw"/utf8>> ->
            <<"application/vnd.sun.xml.writer"/utf8>>;

        <<"t"/utf8>> ->
            <<"text/troff"/utf8>>;

        <<"t3"/utf8>> ->
            <<"application/x-t3vm-image"/utf8>>;

        <<"taglet"/utf8>> ->
            <<"application/vnd.mynfc"/utf8>>;

        <<"tao"/utf8>> ->
            <<"application/vnd.tao.intent-module-archive"/utf8>>;

        <<"tar"/utf8>> ->
            <<"application/x-tar"/utf8>>;

        <<"tcap"/utf8>> ->
            <<"application/vnd.3gpp2.tcap"/utf8>>;

        <<"tcl"/utf8>> ->
            <<"application/x-tcl"/utf8>>;

        <<"teacher"/utf8>> ->
            <<"application/vnd.smart.teacher"/utf8>>;

        <<"tei"/utf8>> ->
            <<"application/tei+xml"/utf8>>;

        <<"teicorpus"/utf8>> ->
            <<"application/tei+xml"/utf8>>;

        <<"tex"/utf8>> ->
            <<"application/x-tex"/utf8>>;

        <<"texi"/utf8>> ->
            <<"application/x-texinfo"/utf8>>;

        <<"texinfo"/utf8>> ->
            <<"application/x-texinfo"/utf8>>;

        <<"text"/utf8>> ->
            <<"text/plain"/utf8>>;

        <<"tfi"/utf8>> ->
            <<"application/thraud+xml"/utf8>>;

        <<"tfm"/utf8>> ->
            <<"application/x-tex-tfm"/utf8>>;

        <<"tga"/utf8>> ->
            <<"image/x-tga"/utf8>>;

        <<"thmx"/utf8>> ->
            <<"application/vnd.ms-officetheme"/utf8>>;

        <<"tif"/utf8>> ->
            <<"image/tiff"/utf8>>;

        <<"tiff"/utf8>> ->
            <<"image/tiff"/utf8>>;

        <<"tmo"/utf8>> ->
            <<"application/vnd.tmobile-livetv"/utf8>>;

        <<"torrent"/utf8>> ->
            <<"application/x-bittorrent"/utf8>>;

        <<"tpl"/utf8>> ->
            <<"application/vnd.groove-tool-template"/utf8>>;

        <<"tpt"/utf8>> ->
            <<"application/vnd.trid.tpt"/utf8>>;

        <<"tr"/utf8>> ->
            <<"text/troff"/utf8>>;

        <<"tra"/utf8>> ->
            <<"application/vnd.trueapp"/utf8>>;

        <<"trm"/utf8>> ->
            <<"application/x-msterminal"/utf8>>;

        <<"ts"/utf8>> ->
            <<"video/mp2t"/utf8>>;

        <<"tsd"/utf8>> ->
            <<"application/timestamped-data"/utf8>>;

        <<"tsv"/utf8>> ->
            <<"text/tab-separated-values"/utf8>>;

        <<"ttc"/utf8>> ->
            <<"font/collection"/utf8>>;

        <<"ttf"/utf8>> ->
            <<"font/ttf"/utf8>>;

        <<"ttl"/utf8>> ->
            <<"text/turtle"/utf8>>;

        <<"twd"/utf8>> ->
            <<"application/vnd.simtech-mindmapper"/utf8>>;

        <<"twds"/utf8>> ->
            <<"application/vnd.simtech-mindmapper"/utf8>>;

        <<"txd"/utf8>> ->
            <<"application/vnd.genomatix.tuxedo"/utf8>>;

        <<"txf"/utf8>> ->
            <<"application/vnd.mobius.txf"/utf8>>;

        <<"txt"/utf8>> ->
            <<"text/plain"/utf8>>;

        <<"u32"/utf8>> ->
            <<"application/x-authorware-bin"/utf8>>;

        <<"udeb"/utf8>> ->
            <<"application/x-debian-package"/utf8>>;

        <<"ufd"/utf8>> ->
            <<"application/vnd.ufdl"/utf8>>;

        <<"ufdl"/utf8>> ->
            <<"application/vnd.ufdl"/utf8>>;

        <<"ulx"/utf8>> ->
            <<"application/x-glulx"/utf8>>;

        <<"umj"/utf8>> ->
            <<"application/vnd.umajin"/utf8>>;

        <<"unityweb"/utf8>> ->
            <<"application/vnd.unity"/utf8>>;

        <<"uoml"/utf8>> ->
            <<"application/vnd.uoml+xml"/utf8>>;

        <<"uri"/utf8>> ->
            <<"text/uri-list"/utf8>>;

        <<"uris"/utf8>> ->
            <<"text/uri-list"/utf8>>;

        <<"urls"/utf8>> ->
            <<"text/uri-list"/utf8>>;

        <<"ustar"/utf8>> ->
            <<"application/x-ustar"/utf8>>;

        <<"utz"/utf8>> ->
            <<"application/vnd.uiq.theme"/utf8>>;

        <<"uu"/utf8>> ->
            <<"text/x-uuencode"/utf8>>;

        <<"uva"/utf8>> ->
            <<"audio/vnd.dece.audio"/utf8>>;

        <<"uvd"/utf8>> ->
            <<"application/vnd.dece.data"/utf8>>;

        <<"uvf"/utf8>> ->
            <<"application/vnd.dece.data"/utf8>>;

        <<"uvg"/utf8>> ->
            <<"image/vnd.dece.graphic"/utf8>>;

        <<"uvh"/utf8>> ->
            <<"video/vnd.dece.hd"/utf8>>;

        <<"uvi"/utf8>> ->
            <<"image/vnd.dece.graphic"/utf8>>;

        <<"uvm"/utf8>> ->
            <<"video/vnd.dece.mobile"/utf8>>;

        <<"uvp"/utf8>> ->
            <<"video/vnd.dece.pd"/utf8>>;

        <<"uvs"/utf8>> ->
            <<"video/vnd.dece.sd"/utf8>>;

        <<"uvt"/utf8>> ->
            <<"application/vnd.dece.ttml+xml"/utf8>>;

        <<"uvu"/utf8>> ->
            <<"video/vnd.uvvu.mp4"/utf8>>;

        <<"uvv"/utf8>> ->
            <<"video/vnd.dece.video"/utf8>>;

        <<"uvva"/utf8>> ->
            <<"audio/vnd.dece.audio"/utf8>>;

        <<"uvvd"/utf8>> ->
            <<"application/vnd.dece.data"/utf8>>;

        <<"uvvf"/utf8>> ->
            <<"application/vnd.dece.data"/utf8>>;

        <<"uvvg"/utf8>> ->
            <<"image/vnd.dece.graphic"/utf8>>;

        <<"uvvh"/utf8>> ->
            <<"video/vnd.dece.hd"/utf8>>;

        <<"uvvi"/utf8>> ->
            <<"image/vnd.dece.graphic"/utf8>>;

        <<"uvvm"/utf8>> ->
            <<"video/vnd.dece.mobile"/utf8>>;

        <<"uvvp"/utf8>> ->
            <<"video/vnd.dece.pd"/utf8>>;

        <<"uvvs"/utf8>> ->
            <<"video/vnd.dece.sd"/utf8>>;

        <<"uvvt"/utf8>> ->
            <<"application/vnd.dece.ttml+xml"/utf8>>;

        <<"uvvu"/utf8>> ->
            <<"video/vnd.uvvu.mp4"/utf8>>;

        <<"uvvv"/utf8>> ->
            <<"video/vnd.dece.video"/utf8>>;

        <<"uvvx"/utf8>> ->
            <<"application/vnd.dece.unspecified"/utf8>>;

        <<"uvvz"/utf8>> ->
            <<"application/vnd.dece.zip"/utf8>>;

        <<"uvx"/utf8>> ->
            <<"application/vnd.dece.unspecified"/utf8>>;

        <<"uvz"/utf8>> ->
            <<"application/vnd.dece.zip"/utf8>>;

        <<"vcard"/utf8>> ->
            <<"text/vcard"/utf8>>;

        <<"vcd"/utf8>> ->
            <<"application/x-cdlink"/utf8>>;

        <<"vcf"/utf8>> ->
            <<"text/x-vcard"/utf8>>;

        <<"vcg"/utf8>> ->
            <<"application/vnd.groove-vcard"/utf8>>;

        <<"vcs"/utf8>> ->
            <<"text/x-vcalendar"/utf8>>;

        <<"vcx"/utf8>> ->
            <<"application/vnd.vcx"/utf8>>;

        <<"vis"/utf8>> ->
            <<"application/vnd.visionary"/utf8>>;

        <<"viv"/utf8>> ->
            <<"video/vnd.vivo"/utf8>>;

        <<"vob"/utf8>> ->
            <<"video/x-ms-vob"/utf8>>;

        <<"vor"/utf8>> ->
            <<"application/vnd.stardivision.writer"/utf8>>;

        <<"vox"/utf8>> ->
            <<"application/x-authorware-bin"/utf8>>;

        <<"vrml"/utf8>> ->
            <<"model/vrml"/utf8>>;

        <<"vsd"/utf8>> ->
            <<"application/vnd.visio"/utf8>>;

        <<"vsf"/utf8>> ->
            <<"application/vnd.vsf"/utf8>>;

        <<"vss"/utf8>> ->
            <<"application/vnd.visio"/utf8>>;

        <<"vst"/utf8>> ->
            <<"application/vnd.visio"/utf8>>;

        <<"vsw"/utf8>> ->
            <<"application/vnd.visio"/utf8>>;

        <<"vtu"/utf8>> ->
            <<"model/vnd.vtu"/utf8>>;

        <<"vxml"/utf8>> ->
            <<"application/voicexml+xml"/utf8>>;

        <<"w3d"/utf8>> ->
            <<"application/x-director"/utf8>>;

        <<"wad"/utf8>> ->
            <<"application/x-doom"/utf8>>;

        <<"wav"/utf8>> ->
            <<"audio/x-wav"/utf8>>;

        <<"wasm"/utf8>> ->
            <<"application/wasm"/utf8>>;

        <<"wax"/utf8>> ->
            <<"audio/x-ms-wax"/utf8>>;

        <<"wbmp"/utf8>> ->
            <<"image/vnd.wap.wbmp"/utf8>>;

        <<"wbs"/utf8>> ->
            <<"application/vnd.criticaltools.wbs+xml"/utf8>>;

        <<"wbxml"/utf8>> ->
            <<"application/vnd.wap.wbxml"/utf8>>;

        <<"wcm"/utf8>> ->
            <<"application/vnd.ms-works"/utf8>>;

        <<"wdb"/utf8>> ->
            <<"application/vnd.ms-works"/utf8>>;

        <<"wdp"/utf8>> ->
            <<"image/vnd.ms-photo"/utf8>>;

        <<"weba"/utf8>> ->
            <<"audio/webm"/utf8>>;

        <<"webm"/utf8>> ->
            <<"video/webm"/utf8>>;

        <<"webp"/utf8>> ->
            <<"image/webp"/utf8>>;

        <<"wg"/utf8>> ->
            <<"application/vnd.pmi.widget"/utf8>>;

        <<"wgt"/utf8>> ->
            <<"application/widget"/utf8>>;

        <<"wks"/utf8>> ->
            <<"application/vnd.ms-works"/utf8>>;

        <<"wm"/utf8>> ->
            <<"video/x-ms-wm"/utf8>>;

        <<"wma"/utf8>> ->
            <<"audio/x-ms-wma"/utf8>>;

        <<"wmd"/utf8>> ->
            <<"application/x-ms-wmd"/utf8>>;

        <<"wmf"/utf8>> ->
            <<"application/x-msmetafile"/utf8>>;

        <<"wml"/utf8>> ->
            <<"text/vnd.wap.wml"/utf8>>;

        <<"wmlc"/utf8>> ->
            <<"application/vnd.wap.wmlc"/utf8>>;

        <<"wmls"/utf8>> ->
            <<"text/vnd.wap.wmlscript"/utf8>>;

        <<"wmlsc"/utf8>> ->
            <<"application/vnd.wap.wmlscriptc"/utf8>>;

        <<"wmv"/utf8>> ->
            <<"video/x-ms-wmv"/utf8>>;

        <<"wmx"/utf8>> ->
            <<"video/x-ms-wmx"/utf8>>;

        <<"wmz"/utf8>> ->
            <<"application/x-msmetafile"/utf8>>;

        <<"woff"/utf8>> ->
            <<"font/woff"/utf8>>;

        <<"woff2"/utf8>> ->
            <<"font/woff2"/utf8>>;

        <<"wpd"/utf8>> ->
            <<"application/vnd.wordperfect"/utf8>>;

        <<"wpl"/utf8>> ->
            <<"application/vnd.ms-wpl"/utf8>>;

        <<"wps"/utf8>> ->
            <<"application/vnd.ms-works"/utf8>>;

        <<"wqd"/utf8>> ->
            <<"application/vnd.wqd"/utf8>>;

        <<"wri"/utf8>> ->
            <<"application/x-mswrite"/utf8>>;

        <<"wrl"/utf8>> ->
            <<"model/vrml"/utf8>>;

        <<"wsdl"/utf8>> ->
            <<"application/wsdl+xml"/utf8>>;

        <<"wspolicy"/utf8>> ->
            <<"application/wspolicy+xml"/utf8>>;

        <<"wtb"/utf8>> ->
            <<"application/vnd.webturbo"/utf8>>;

        <<"wvx"/utf8>> ->
            <<"video/x-ms-wvx"/utf8>>;

        <<"x32"/utf8>> ->
            <<"application/x-authorware-bin"/utf8>>;

        <<"x3d"/utf8>> ->
            <<"model/x3d+xml"/utf8>>;

        <<"x3db"/utf8>> ->
            <<"model/x3d+binary"/utf8>>;

        <<"x3dbz"/utf8>> ->
            <<"model/x3d+binary"/utf8>>;

        <<"x3dv"/utf8>> ->
            <<"model/x3d+vrml"/utf8>>;

        <<"x3dvz"/utf8>> ->
            <<"model/x3d+vrml"/utf8>>;

        <<"x3dz"/utf8>> ->
            <<"model/x3d+xml"/utf8>>;

        <<"xaml"/utf8>> ->
            <<"application/xaml+xml"/utf8>>;

        <<"xap"/utf8>> ->
            <<"application/x-silverlight-app"/utf8>>;

        <<"xar"/utf8>> ->
            <<"application/vnd.xara"/utf8>>;

        <<"xbap"/utf8>> ->
            <<"application/x-ms-xbap"/utf8>>;

        <<"xbd"/utf8>> ->
            <<"application/vnd.fujixerox.docuworks.binder"/utf8>>;

        <<"xbm"/utf8>> ->
            <<"image/x-xbitmap"/utf8>>;

        <<"xdf"/utf8>> ->
            <<"application/xcap-diff+xml"/utf8>>;

        <<"xdm"/utf8>> ->
            <<"application/vnd.syncml.dm+xml"/utf8>>;

        <<"xdp"/utf8>> ->
            <<"application/vnd.adobe.xdp+xml"/utf8>>;

        <<"xdssc"/utf8>> ->
            <<"application/dssc+xml"/utf8>>;

        <<"xdw"/utf8>> ->
            <<"application/vnd.fujixerox.docuworks"/utf8>>;

        <<"xenc"/utf8>> ->
            <<"application/xenc+xml"/utf8>>;

        <<"xer"/utf8>> ->
            <<"application/patch-ops-error+xml"/utf8>>;

        <<"xfdf"/utf8>> ->
            <<"application/vnd.adobe.xfdf"/utf8>>;

        <<"xfdl"/utf8>> ->
            <<"application/vnd.xfdl"/utf8>>;

        <<"xht"/utf8>> ->
            <<"application/xhtml+xml"/utf8>>;

        <<"xhtml"/utf8>> ->
            <<"application/xhtml+xml"/utf8>>;

        <<"xhvml"/utf8>> ->
            <<"application/xv+xml"/utf8>>;

        <<"xif"/utf8>> ->
            <<"image/vnd.xiff"/utf8>>;

        <<"xla"/utf8>> ->
            <<"application/vnd.ms-excel"/utf8>>;

        <<"xlam"/utf8>> ->
            <<"application/vnd.ms-excel.addin.macroenabled.12"/utf8>>;

        <<"xlc"/utf8>> ->
            <<"application/vnd.ms-excel"/utf8>>;

        <<"xlf"/utf8>> ->
            <<"application/x-xliff+xml"/utf8>>;

        <<"xlm"/utf8>> ->
            <<"application/vnd.ms-excel"/utf8>>;

        <<"xls"/utf8>> ->
            <<"application/vnd.ms-excel"/utf8>>;

        <<"xlsb"/utf8>> ->
            <<"application/vnd.ms-excel.sheet.binary.macroenabled.12"/utf8>>;

        <<"xlsm"/utf8>> ->
            <<"application/vnd.ms-excel.sheet.macroenabled.12"/utf8>>;

        <<"xlsx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"/utf8>>;

        <<"xlt"/utf8>> ->
            <<"application/vnd.ms-excel"/utf8>>;

        <<"xltm"/utf8>> ->
            <<"application/vnd.ms-excel.template.macroenabled.12"/utf8>>;

        <<"xltx"/utf8>> ->
            <<"application/vnd.openxmlformats-officedocument.spreadsheetml.template"/utf8>>;

        <<"xlw"/utf8>> ->
            <<"application/vnd.ms-excel"/utf8>>;

        <<"xm"/utf8>> ->
            <<"audio/xm"/utf8>>;

        <<"xml"/utf8>> ->
            <<"application/xml"/utf8>>;

        <<"xo"/utf8>> ->
            <<"application/vnd.olpc-sugar"/utf8>>;

        <<"xop"/utf8>> ->
            <<"application/xop+xml"/utf8>>;

        <<"xpi"/utf8>> ->
            <<"application/x-xpinstall"/utf8>>;

        <<"xpl"/utf8>> ->
            <<"application/xproc+xml"/utf8>>;

        <<"xpm"/utf8>> ->
            <<"image/x-xpixmap"/utf8>>;

        <<"xpr"/utf8>> ->
            <<"application/vnd.is-xpr"/utf8>>;

        <<"xps"/utf8>> ->
            <<"application/vnd.ms-xpsdocument"/utf8>>;

        <<"xpw"/utf8>> ->
            <<"application/vnd.intercon.formnet"/utf8>>;

        <<"xpx"/utf8>> ->
            <<"application/vnd.intercon.formnet"/utf8>>;

        <<"xsl"/utf8>> ->
            <<"application/xml"/utf8>>;

        <<"xslt"/utf8>> ->
            <<"application/xslt+xml"/utf8>>;

        <<"xsm"/utf8>> ->
            <<"application/vnd.syncml+xml"/utf8>>;

        <<"xspf"/utf8>> ->
            <<"application/xspf+xml"/utf8>>;

        <<"xul"/utf8>> ->
            <<"application/vnd.mozilla.xul+xml"/utf8>>;

        <<"xvm"/utf8>> ->
            <<"application/xv+xml"/utf8>>;

        <<"xvml"/utf8>> ->
            <<"application/xv+xml"/utf8>>;

        <<"xwd"/utf8>> ->
            <<"image/x-xwindowdump"/utf8>>;

        <<"xyz"/utf8>> ->
            <<"chemical/x-xyz"/utf8>>;

        <<"xz"/utf8>> ->
            <<"application/x-xz"/utf8>>;

        <<"yang"/utf8>> ->
            <<"application/yang"/utf8>>;

        <<"yin"/utf8>> ->
            <<"application/yin+xml"/utf8>>;

        <<"z1"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"z2"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"z3"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"z4"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"z5"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"z6"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"z7"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"z8"/utf8>> ->
            <<"application/x-zmachine"/utf8>>;

        <<"zaz"/utf8>> ->
            <<"application/vnd.zzazz.deck+xml"/utf8>>;

        <<"zip"/utf8>> ->
            <<"application/zip"/utf8>>;

        <<"zir"/utf8>> ->
            <<"application/vnd.zul"/utf8>>;

        <<"zirz"/utf8>> ->
            <<"application/vnd.zul"/utf8>>;

        <<"zmm"/utf8>> ->
            <<"application/vnd.handheld-entertainment+xml"/utf8>>;

        _ ->
            <<"application/octet-stream"/utf8>>
    end.

-file("/Users/louis/src/gleam/marceau/src/marceau.gleam", 1007).
-spec mime_type_to_extensions(binary()) -> list(binary()).
mime_type_to_extensions(Mime_type) ->
    case Mime_type of
        <<"application/andrew-inset"/utf8>> ->
            [<<"ez"/utf8>>];

        <<"application/applixware"/utf8>> ->
            [<<"aw"/utf8>>];

        <<"application/atom+xml"/utf8>> ->
            [<<"atom"/utf8>>];

        <<"application/atomcat+xml"/utf8>> ->
            [<<"atomcat"/utf8>>];

        <<"application/atomsvc+xml"/utf8>> ->
            [<<"atomsvc"/utf8>>];

        <<"application/ccxml+xml"/utf8>> ->
            [<<"ccxml"/utf8>>];

        <<"application/cdmi-capability"/utf8>> ->
            [<<"cdmia"/utf8>>];

        <<"application/cdmi-container"/utf8>> ->
            [<<"cdmic"/utf8>>];

        <<"application/cdmi-domain"/utf8>> ->
            [<<"cdmid"/utf8>>];

        <<"application/cdmi-object"/utf8>> ->
            [<<"cdmio"/utf8>>];

        <<"application/cdmi-queue"/utf8>> ->
            [<<"cdmiq"/utf8>>];

        <<"application/cu-seeme"/utf8>> ->
            [<<"cu"/utf8>>];

        <<"application/davmount+xml"/utf8>> ->
            [<<"davmount"/utf8>>];

        <<"application/docbook+xml"/utf8>> ->
            [<<"dbk"/utf8>>];

        <<"application/dssc+der"/utf8>> ->
            [<<"dssc"/utf8>>];

        <<"application/dssc+xml"/utf8>> ->
            [<<"xdssc"/utf8>>];

        <<"application/ecmascript"/utf8>> ->
            [<<"ecma"/utf8>>];

        <<"application/emma+xml"/utf8>> ->
            [<<"emma"/utf8>>];

        <<"application/epub+zip"/utf8>> ->
            [<<"epub"/utf8>>];

        <<"application/exi"/utf8>> ->
            [<<"exi"/utf8>>];

        <<"application/font-tdpfr"/utf8>> ->
            [<<"pfr"/utf8>>];

        <<"application/gml+xml"/utf8>> ->
            [<<"gml"/utf8>>];

        <<"application/gpx+xml"/utf8>> ->
            [<<"gpx"/utf8>>];

        <<"application/gxf"/utf8>> ->
            [<<"gxf"/utf8>>];

        <<"application/hyperstudio"/utf8>> ->
            [<<"stk"/utf8>>];

        <<"application/inkml+xml"/utf8>> ->
            [<<"ink"/utf8>>, <<"inkml"/utf8>>];

        <<"application/ipfix"/utf8>> ->
            [<<"ipfix"/utf8>>];

        <<"application/java-archive"/utf8>> ->
            [<<"jar"/utf8>>];

        <<"application/java-serialized-object"/utf8>> ->
            [<<"ser"/utf8>>];

        <<"application/java-vm"/utf8>> ->
            [<<"class"/utf8>>];

        <<"application/javascript"/utf8>> ->
            [<<"js"/utf8>>, <<"mjs"/utf8>>];

        <<"application/json"/utf8>> ->
            [<<"json"/utf8>>];

        <<"application/jsonml+json"/utf8>> ->
            [<<"jsonml"/utf8>>];

        <<"application/lost+xml"/utf8>> ->
            [<<"lostxml"/utf8>>];

        <<"application/mac-binhex40"/utf8>> ->
            [<<"hqx"/utf8>>];

        <<"application/mac-compactpro"/utf8>> ->
            [<<"cpt"/utf8>>];

        <<"application/mads+xml"/utf8>> ->
            [<<"mads"/utf8>>];

        <<"application/marc"/utf8>> ->
            [<<"mrc"/utf8>>];

        <<"application/marcxml+xml"/utf8>> ->
            [<<"mrcx"/utf8>>];

        <<"application/mathematica"/utf8>> ->
            [<<"ma"/utf8>>, <<"nb"/utf8>>, <<"mb"/utf8>>];

        <<"application/mathml+xml"/utf8>> ->
            [<<"mathml"/utf8>>];

        <<"application/mbox"/utf8>> ->
            [<<"mbox"/utf8>>];

        <<"application/mediaservercontrol+xml"/utf8>> ->
            [<<"mscml"/utf8>>];

        <<"application/metalink+xml"/utf8>> ->
            [<<"metalink"/utf8>>];

        <<"application/metalink4+xml"/utf8>> ->
            [<<"meta4"/utf8>>];

        <<"application/mets+xml"/utf8>> ->
            [<<"mets"/utf8>>];

        <<"application/mods+xml"/utf8>> ->
            [<<"mods"/utf8>>];

        <<"application/mp21"/utf8>> ->
            [<<"m21"/utf8>>, <<"mp21"/utf8>>];

        <<"application/mp4"/utf8>> ->
            [<<"mp4s"/utf8>>];

        <<"application/msword"/utf8>> ->
            [<<"doc"/utf8>>, <<"dot"/utf8>>];

        <<"application/mxf"/utf8>> ->
            [<<"mxf"/utf8>>];

        <<"application/octet-stream"/utf8>> ->
            [<<"bin"/utf8>>,
                <<"dms"/utf8>>,
                <<"lrf"/utf8>>,
                <<"mar"/utf8>>,
                <<"so"/utf8>>,
                <<"dist"/utf8>>,
                <<"distz"/utf8>>,
                <<"pkg"/utf8>>,
                <<"bpk"/utf8>>,
                <<"dump"/utf8>>,
                <<"elc"/utf8>>,
                <<"deploy"/utf8>>];

        <<"application/oda"/utf8>> ->
            [<<"oda"/utf8>>];

        <<"application/oebps-package+xml"/utf8>> ->
            [<<"opf"/utf8>>];

        <<"application/ogg"/utf8>> ->
            [<<"ogx"/utf8>>];

        <<"application/omdoc+xml"/utf8>> ->
            [<<"omdoc"/utf8>>];

        <<"application/onenote"/utf8>> ->
            [<<"onetoc"/utf8>>,
                <<"onetoc2"/utf8>>,
                <<"onetmp"/utf8>>,
                <<"onepkg"/utf8>>];

        <<"application/oxps"/utf8>> ->
            [<<"oxps"/utf8>>];

        <<"application/patch-ops-error+xml"/utf8>> ->
            [<<"xer"/utf8>>];

        <<"application/pdf"/utf8>> ->
            [<<"pdf"/utf8>>];

        <<"application/pgp-encrypted"/utf8>> ->
            [<<"pgp"/utf8>>];

        <<"application/pgp-signature"/utf8>> ->
            [<<"asc"/utf8>>, <<"sig"/utf8>>];

        <<"application/pics-rules"/utf8>> ->
            [<<"prf"/utf8>>];

        <<"application/pkcs10"/utf8>> ->
            [<<"p10"/utf8>>];

        <<"application/pkcs7-mime"/utf8>> ->
            [<<"p7m"/utf8>>, <<"p7c"/utf8>>];

        <<"application/pkcs7-signature"/utf8>> ->
            [<<"p7s"/utf8>>];

        <<"application/pkcs8"/utf8>> ->
            [<<"p8"/utf8>>];

        <<"application/pkix-attr-cert"/utf8>> ->
            [<<"ac"/utf8>>];

        <<"application/pkix-cert"/utf8>> ->
            [<<"cer"/utf8>>];

        <<"application/pkix-crl"/utf8>> ->
            [<<"crl"/utf8>>];

        <<"application/pkix-pkipath"/utf8>> ->
            [<<"pkipath"/utf8>>];

        <<"application/pkixcmp"/utf8>> ->
            [<<"pki"/utf8>>];

        <<"application/pls+xml"/utf8>> ->
            [<<"pls"/utf8>>];

        <<"application/postscript"/utf8>> ->
            [<<"ai"/utf8>>, <<"eps"/utf8>>, <<"ps"/utf8>>];

        <<"application/prs.cww"/utf8>> ->
            [<<"cww"/utf8>>];

        <<"application/pskc+xml"/utf8>> ->
            [<<"pskcxml"/utf8>>];

        <<"application/rdf+xml"/utf8>> ->
            [<<"rdf"/utf8>>];

        <<"application/reginfo+xml"/utf8>> ->
            [<<"rif"/utf8>>];

        <<"application/relax-ng-compact-syntax"/utf8>> ->
            [<<"rnc"/utf8>>];

        <<"application/resource-lists+xml"/utf8>> ->
            [<<"rl"/utf8>>];

        <<"application/resource-lists-diff+xml"/utf8>> ->
            [<<"rld"/utf8>>];

        <<"application/rls-services+xml"/utf8>> ->
            [<<"rs"/utf8>>];

        <<"application/rpki-ghostbusters"/utf8>> ->
            [<<"gbr"/utf8>>];

        <<"application/rpki-manifest"/utf8>> ->
            [<<"mft"/utf8>>];

        <<"application/rpki-roa"/utf8>> ->
            [<<"roa"/utf8>>];

        <<"application/rsd+xml"/utf8>> ->
            [<<"rsd"/utf8>>];

        <<"application/rss+xml"/utf8>> ->
            [<<"rss"/utf8>>];

        <<"application/rtf"/utf8>> ->
            [<<"rtf"/utf8>>];

        <<"application/sbml+xml"/utf8>> ->
            [<<"sbml"/utf8>>];

        <<"application/scvp-cv-request"/utf8>> ->
            [<<"scq"/utf8>>];

        <<"application/scvp-cv-response"/utf8>> ->
            [<<"scs"/utf8>>];

        <<"application/scvp-vp-request"/utf8>> ->
            [<<"spq"/utf8>>];

        <<"application/scvp-vp-response"/utf8>> ->
            [<<"spp"/utf8>>];

        <<"application/sdp"/utf8>> ->
            [<<"sdp"/utf8>>];

        <<"application/set-payment-initiation"/utf8>> ->
            [<<"setpay"/utf8>>];

        <<"application/set-registration-initiation"/utf8>> ->
            [<<"setreg"/utf8>>];

        <<"application/shf+xml"/utf8>> ->
            [<<"shf"/utf8>>];

        <<"application/smil+xml"/utf8>> ->
            [<<"smi"/utf8>>, <<"smil"/utf8>>];

        <<"application/sparql-query"/utf8>> ->
            [<<"rq"/utf8>>];

        <<"application/sparql-results+xml"/utf8>> ->
            [<<"srx"/utf8>>];

        <<"application/srgs"/utf8>> ->
            [<<"gram"/utf8>>];

        <<"application/srgs+xml"/utf8>> ->
            [<<"grxml"/utf8>>];

        <<"application/sru+xml"/utf8>> ->
            [<<"sru"/utf8>>];

        <<"application/ssdl+xml"/utf8>> ->
            [<<"ssdl"/utf8>>];

        <<"application/ssml+xml"/utf8>> ->
            [<<"ssml"/utf8>>];

        <<"application/tei+xml"/utf8>> ->
            [<<"tei"/utf8>>, <<"teicorpus"/utf8>>];

        <<"application/thraud+xml"/utf8>> ->
            [<<"tfi"/utf8>>];

        <<"application/timestamped-data"/utf8>> ->
            [<<"tsd"/utf8>>];

        <<"application/vnd.3gpp.pic-bw-large"/utf8>> ->
            [<<"plb"/utf8>>];

        <<"application/vnd.3gpp.pic-bw-small"/utf8>> ->
            [<<"psb"/utf8>>];

        <<"application/vnd.3gpp.pic-bw-var"/utf8>> ->
            [<<"pvb"/utf8>>];

        <<"application/vnd.3gpp2.tcap"/utf8>> ->
            [<<"tcap"/utf8>>];

        <<"application/vnd.3m.post-it-notes"/utf8>> ->
            [<<"pwn"/utf8>>];

        <<"application/vnd.accpac.simply.aso"/utf8>> ->
            [<<"aso"/utf8>>];

        <<"application/vnd.accpac.simply.imp"/utf8>> ->
            [<<"imp"/utf8>>];

        <<"application/vnd.acucobol"/utf8>> ->
            [<<"acu"/utf8>>];

        <<"application/vnd.acucorp"/utf8>> ->
            [<<"atc"/utf8>>, <<"acutc"/utf8>>];

        <<"application/vnd.adobe.air-application-installer-package+zip"/utf8>> ->
            [<<"air"/utf8>>];

        <<"application/vnd.adobe.formscentral.fcdt"/utf8>> ->
            [<<"fcdt"/utf8>>];

        <<"application/vnd.adobe.fxp"/utf8>> ->
            [<<"fxp"/utf8>>, <<"fxpl"/utf8>>];

        <<"application/vnd.adobe.xdp+xml"/utf8>> ->
            [<<"xdp"/utf8>>];

        <<"application/vnd.adobe.xfdf"/utf8>> ->
            [<<"xfdf"/utf8>>];

        <<"application/vnd.ahead.space"/utf8>> ->
            [<<"ahead"/utf8>>];

        <<"application/vnd.airzip.filesecure.azf"/utf8>> ->
            [<<"azf"/utf8>>];

        <<"application/vnd.airzip.filesecure.azs"/utf8>> ->
            [<<"azs"/utf8>>];

        <<"application/vnd.amazon.ebook"/utf8>> ->
            [<<"azw"/utf8>>];

        <<"application/vnd.americandynamics.acc"/utf8>> ->
            [<<"acc"/utf8>>];

        <<"application/vnd.amiga.ami"/utf8>> ->
            [<<"ami"/utf8>>];

        <<"application/vnd.android.package-archive"/utf8>> ->
            [<<"apk"/utf8>>];

        <<"application/vnd.anser-web-certificate-issue-initiation"/utf8>> ->
            [<<"cii"/utf8>>];

        <<"application/vnd.anser-web-funds-transfer-initiation"/utf8>> ->
            [<<"fti"/utf8>>];

        <<"application/vnd.antix.game-component"/utf8>> ->
            [<<"atx"/utf8>>];

        <<"application/vnd.apple.installer+xml"/utf8>> ->
            [<<"mpkg"/utf8>>];

        <<"application/vnd.apple.mpegurl"/utf8>> ->
            [<<"m3u8"/utf8>>];

        <<"application/vnd.aristanetworks.swi"/utf8>> ->
            [<<"swi"/utf8>>];

        <<"application/vnd.astraea-software.iota"/utf8>> ->
            [<<"iota"/utf8>>];

        <<"application/vnd.audiograph"/utf8>> ->
            [<<"aep"/utf8>>];

        <<"application/vnd.blueice.multipass"/utf8>> ->
            [<<"mpm"/utf8>>];

        <<"application/vnd.bmi"/utf8>> ->
            [<<"bmi"/utf8>>];

        <<"application/vnd.businessobjects"/utf8>> ->
            [<<"rep"/utf8>>];

        <<"application/vnd.chemdraw+xml"/utf8>> ->
            [<<"cdxml"/utf8>>];

        <<"application/vnd.chipnuts.karaoke-mmd"/utf8>> ->
            [<<"mmd"/utf8>>];

        <<"application/vnd.cinderella"/utf8>> ->
            [<<"cdy"/utf8>>];

        <<"application/vnd.claymore"/utf8>> ->
            [<<"cla"/utf8>>];

        <<"application/vnd.cloanto.rp9"/utf8>> ->
            [<<"rp9"/utf8>>];

        <<"application/vnd.clonk.c4group"/utf8>> ->
            [<<"c4g"/utf8>>,
                <<"c4d"/utf8>>,
                <<"c4f"/utf8>>,
                <<"c4p"/utf8>>,
                <<"c4u"/utf8>>];

        <<"application/vnd.cluetrust.cartomobile-config"/utf8>> ->
            [<<"c11amc"/utf8>>];

        <<"application/vnd.cluetrust.cartomobile-config-pkg"/utf8>> ->
            [<<"c11amz"/utf8>>];

        <<"application/vnd.commonspace"/utf8>> ->
            [<<"csp"/utf8>>];

        <<"application/vnd.contact.cmsg"/utf8>> ->
            [<<"cdbcmsg"/utf8>>];

        <<"application/vnd.cosmocaller"/utf8>> ->
            [<<"cmc"/utf8>>];

        <<"application/vnd.crick.clicker"/utf8>> ->
            [<<"clkx"/utf8>>];

        <<"application/vnd.crick.clicker.keyboard"/utf8>> ->
            [<<"clkk"/utf8>>];

        <<"application/vnd.crick.clicker.palette"/utf8>> ->
            [<<"clkp"/utf8>>];

        <<"application/vnd.crick.clicker.template"/utf8>> ->
            [<<"clkt"/utf8>>];

        <<"application/vnd.crick.clicker.wordbank"/utf8>> ->
            [<<"clkw"/utf8>>];

        <<"application/vnd.criticaltools.wbs+xml"/utf8>> ->
            [<<"wbs"/utf8>>];

        <<"application/vnd.ctc-posml"/utf8>> ->
            [<<"pml"/utf8>>];

        <<"application/vnd.cups-ppd"/utf8>> ->
            [<<"ppd"/utf8>>];

        <<"application/vnd.curl.car"/utf8>> ->
            [<<"car"/utf8>>];

        <<"application/vnd.curl.pcurl"/utf8>> ->
            [<<"pcurl"/utf8>>];

        <<"application/vnd.dart"/utf8>> ->
            [<<"dart"/utf8>>];

        <<"application/vnd.data-vision.rdz"/utf8>> ->
            [<<"rdz"/utf8>>];

        <<"application/vnd.dece.data"/utf8>> ->
            [<<"uvf"/utf8>>, <<"uvvf"/utf8>>, <<"uvd"/utf8>>, <<"uvvd"/utf8>>];

        <<"application/vnd.dece.ttml+xml"/utf8>> ->
            [<<"uvt"/utf8>>, <<"uvvt"/utf8>>];

        <<"application/vnd.dece.unspecified"/utf8>> ->
            [<<"uvx"/utf8>>, <<"uvvx"/utf8>>];

        <<"application/vnd.dece.zip"/utf8>> ->
            [<<"uvz"/utf8>>, <<"uvvz"/utf8>>];

        <<"application/vnd.denovo.fcselayout-link"/utf8>> ->
            [<<"fe_launch"/utf8>>];

        <<"application/vnd.dna"/utf8>> ->
            [<<"dna"/utf8>>];

        <<"application/vnd.dolby.mlp"/utf8>> ->
            [<<"mlp"/utf8>>];

        <<"application/vnd.dpgraph"/utf8>> ->
            [<<"dpg"/utf8>>];

        <<"application/vnd.dreamfactory"/utf8>> ->
            [<<"dfac"/utf8>>];

        <<"application/vnd.ds-keypoint"/utf8>> ->
            [<<"kpxx"/utf8>>];

        <<"application/vnd.dvb.ait"/utf8>> ->
            [<<"ait"/utf8>>];

        <<"application/vnd.dvb.service"/utf8>> ->
            [<<"svc"/utf8>>];

        <<"application/vnd.dynageo"/utf8>> ->
            [<<"geo"/utf8>>];

        <<"application/vnd.ecowin.chart"/utf8>> ->
            [<<"mag"/utf8>>];

        <<"application/vnd.enliven"/utf8>> ->
            [<<"nml"/utf8>>];

        <<"application/vnd.epson.esf"/utf8>> ->
            [<<"esf"/utf8>>];

        <<"application/vnd.epson.msf"/utf8>> ->
            [<<"msf"/utf8>>];

        <<"application/vnd.epson.quickanime"/utf8>> ->
            [<<"qam"/utf8>>];

        <<"application/vnd.epson.salt"/utf8>> ->
            [<<"slt"/utf8>>];

        <<"application/vnd.epson.ssf"/utf8>> ->
            [<<"ssf"/utf8>>];

        <<"application/vnd.eszigno3+xml"/utf8>> ->
            [<<"es3"/utf8>>, <<"et3"/utf8>>];

        <<"application/vnd.ezpix-album"/utf8>> ->
            [<<"ez2"/utf8>>];

        <<"application/vnd.ezpix-package"/utf8>> ->
            [<<"ez3"/utf8>>];

        <<"application/vnd.fdf"/utf8>> ->
            [<<"fdf"/utf8>>];

        <<"application/vnd.fdsn.mseed"/utf8>> ->
            [<<"mseed"/utf8>>];

        <<"application/vnd.fdsn.seed"/utf8>> ->
            [<<"seed"/utf8>>, <<"dataless"/utf8>>];

        <<"application/vnd.flographit"/utf8>> ->
            [<<"gph"/utf8>>];

        <<"application/vnd.fluxtime.clip"/utf8>> ->
            [<<"ftc"/utf8>>];

        <<"application/vnd.framemaker"/utf8>> ->
            [<<"fm"/utf8>>, <<"frame"/utf8>>, <<"maker"/utf8>>, <<"book"/utf8>>];

        <<"application/vnd.frogans.fnc"/utf8>> ->
            [<<"fnc"/utf8>>];

        <<"application/vnd.frogans.ltf"/utf8>> ->
            [<<"ltf"/utf8>>];

        <<"application/vnd.fsc.weblaunch"/utf8>> ->
            [<<"fsc"/utf8>>];

        <<"application/vnd.fujitsu.oasys"/utf8>> ->
            [<<"oas"/utf8>>];

        <<"application/vnd.fujitsu.oasys2"/utf8>> ->
            [<<"oa2"/utf8>>];

        <<"application/vnd.fujitsu.oasys3"/utf8>> ->
            [<<"oa3"/utf8>>];

        <<"application/vnd.fujitsu.oasysgp"/utf8>> ->
            [<<"fg5"/utf8>>];

        <<"application/vnd.fujitsu.oasysprs"/utf8>> ->
            [<<"bh2"/utf8>>];

        <<"application/vnd.fujixerox.ddd"/utf8>> ->
            [<<"ddd"/utf8>>];

        <<"application/vnd.fujixerox.docuworks"/utf8>> ->
            [<<"xdw"/utf8>>];

        <<"application/vnd.fujixerox.docuworks.binder"/utf8>> ->
            [<<"xbd"/utf8>>];

        <<"application/vnd.fuzzysheet"/utf8>> ->
            [<<"fzs"/utf8>>];

        <<"application/vnd.genomatix.tuxedo"/utf8>> ->
            [<<"txd"/utf8>>];

        <<"application/vnd.geogebra.file"/utf8>> ->
            [<<"ggb"/utf8>>];

        <<"application/vnd.geogebra.slides"/utf8>> ->
            [<<"ggs"/utf8>>];

        <<"application/vnd.geogebra.tool"/utf8>> ->
            [<<"ggt"/utf8>>];

        <<"application/vnd.geometry-explorer"/utf8>> ->
            [<<"gex"/utf8>>, <<"gre"/utf8>>];

        <<"application/vnd.geonext"/utf8>> ->
            [<<"gxt"/utf8>>];

        <<"application/vnd.geoplan"/utf8>> ->
            [<<"g2w"/utf8>>];

        <<"application/vnd.geospace"/utf8>> ->
            [<<"g3w"/utf8>>];

        <<"application/vnd.gmx"/utf8>> ->
            [<<"gmx"/utf8>>];

        <<"application/vnd.google-earth.kml+xml"/utf8>> ->
            [<<"kml"/utf8>>];

        <<"application/vnd.google-earth.kmz"/utf8>> ->
            [<<"kmz"/utf8>>];

        <<"application/vnd.grafeq"/utf8>> ->
            [<<"gqf"/utf8>>, <<"gqs"/utf8>>];

        <<"application/vnd.groove-account"/utf8>> ->
            [<<"gac"/utf8>>];

        <<"application/vnd.groove-help"/utf8>> ->
            [<<"ghf"/utf8>>];

        <<"application/vnd.groove-identity-message"/utf8>> ->
            [<<"gim"/utf8>>];

        <<"application/vnd.groove-injector"/utf8>> ->
            [<<"grv"/utf8>>];

        <<"application/vnd.groove-tool-message"/utf8>> ->
            [<<"gtm"/utf8>>];

        <<"application/vnd.groove-tool-template"/utf8>> ->
            [<<"tpl"/utf8>>];

        <<"application/vnd.groove-vcard"/utf8>> ->
            [<<"vcg"/utf8>>];

        <<"application/vnd.hal+xml"/utf8>> ->
            [<<"hal"/utf8>>];

        <<"application/vnd.handheld-entertainment+xml"/utf8>> ->
            [<<"zmm"/utf8>>];

        <<"application/vnd.hbci"/utf8>> ->
            [<<"hbci"/utf8>>];

        <<"application/vnd.hhe.lesson-player"/utf8>> ->
            [<<"les"/utf8>>];

        <<"application/vnd.hp-hpgl"/utf8>> ->
            [<<"hpgl"/utf8>>];

        <<"application/vnd.hp-hpid"/utf8>> ->
            [<<"hpid"/utf8>>];

        <<"application/vnd.hp-hps"/utf8>> ->
            [<<"hps"/utf8>>];

        <<"application/vnd.hp-jlyt"/utf8>> ->
            [<<"jlt"/utf8>>];

        <<"application/vnd.hp-pcl"/utf8>> ->
            [<<"pcl"/utf8>>];

        <<"application/vnd.hp-pclxl"/utf8>> ->
            [<<"pclxl"/utf8>>];

        <<"application/vnd.hydrostatix.sof-data"/utf8>> ->
            [<<"sfd-hdstx"/utf8>>];

        <<"application/vnd.ibm.minipay"/utf8>> ->
            [<<"mpy"/utf8>>];

        <<"application/vnd.ibm.modcap"/utf8>> ->
            [<<"afp"/utf8>>, <<"listafp"/utf8>>, <<"list3820"/utf8>>];

        <<"application/vnd.ibm.rights-management"/utf8>> ->
            [<<"irm"/utf8>>];

        <<"application/vnd.ibm.secure-container"/utf8>> ->
            [<<"sc"/utf8>>];

        <<"application/vnd.iccprofile"/utf8>> ->
            [<<"icc"/utf8>>, <<"icm"/utf8>>];

        <<"application/vnd.igloader"/utf8>> ->
            [<<"igl"/utf8>>];

        <<"application/vnd.immervision-ivp"/utf8>> ->
            [<<"ivp"/utf8>>];

        <<"application/vnd.immervision-ivu"/utf8>> ->
            [<<"ivu"/utf8>>];

        <<"application/vnd.insors.igm"/utf8>> ->
            [<<"igm"/utf8>>];

        <<"application/vnd.intercon.formnet"/utf8>> ->
            [<<"xpw"/utf8>>, <<"xpx"/utf8>>];

        <<"application/vnd.intergeo"/utf8>> ->
            [<<"i2g"/utf8>>];

        <<"application/vnd.intu.qbo"/utf8>> ->
            [<<"qbo"/utf8>>];

        <<"application/vnd.intu.qfx"/utf8>> ->
            [<<"qfx"/utf8>>];

        <<"application/vnd.ipunplugged.rcprofile"/utf8>> ->
            [<<"rcprofile"/utf8>>];

        <<"application/vnd.irepository.package+xml"/utf8>> ->
            [<<"irp"/utf8>>];

        <<"application/vnd.is-xpr"/utf8>> ->
            [<<"xpr"/utf8>>];

        <<"application/vnd.isac.fcs"/utf8>> ->
            [<<"fcs"/utf8>>];

        <<"application/vnd.jam"/utf8>> ->
            [<<"jam"/utf8>>];

        <<"application/vnd.jcp.javame.midlet-rms"/utf8>> ->
            [<<"rms"/utf8>>];

        <<"application/vnd.jisp"/utf8>> ->
            [<<"jisp"/utf8>>];

        <<"application/vnd.joost.joda-archive"/utf8>> ->
            [<<"joda"/utf8>>];

        <<"application/vnd.kahootz"/utf8>> ->
            [<<"ktz"/utf8>>, <<"ktr"/utf8>>];

        <<"application/vnd.kde.karbon"/utf8>> ->
            [<<"karbon"/utf8>>];

        <<"application/vnd.kde.kchart"/utf8>> ->
            [<<"chrt"/utf8>>];

        <<"application/vnd.kde.kformula"/utf8>> ->
            [<<"kfo"/utf8>>];

        <<"application/vnd.kde.kivio"/utf8>> ->
            [<<"flw"/utf8>>];

        <<"application/vnd.kde.kontour"/utf8>> ->
            [<<"kon"/utf8>>];

        <<"application/vnd.kde.kpresenter"/utf8>> ->
            [<<"kpr"/utf8>>, <<"kpt"/utf8>>];

        <<"application/vnd.kde.kspread"/utf8>> ->
            [<<"ksp"/utf8>>];

        <<"application/vnd.kde.kword"/utf8>> ->
            [<<"kwd"/utf8>>, <<"kwt"/utf8>>];

        <<"application/vnd.kenameaapp"/utf8>> ->
            [<<"htke"/utf8>>];

        <<"application/vnd.kidspiration"/utf8>> ->
            [<<"kia"/utf8>>];

        <<"application/vnd.kinar"/utf8>> ->
            [<<"kne"/utf8>>, <<"knp"/utf8>>];

        <<"application/vnd.koan"/utf8>> ->
            [<<"skp"/utf8>>, <<"skd"/utf8>>, <<"skt"/utf8>>, <<"skm"/utf8>>];

        <<"application/vnd.kodak-descriptor"/utf8>> ->
            [<<"sse"/utf8>>];

        <<"application/vnd.las.las+xml"/utf8>> ->
            [<<"lasxml"/utf8>>];

        <<"application/vnd.llamagraphics.life-balance.desktop"/utf8>> ->
            [<<"lbd"/utf8>>];

        <<"application/vnd.llamagraphics.life-balance.exchange+xml"/utf8>> ->
            [<<"lbe"/utf8>>];

        <<"application/vnd.lotus-1-2-3"/utf8>> ->
            [<<"123"/utf8>>];

        <<"application/vnd.lotus-approach"/utf8>> ->
            [<<"apr"/utf8>>];

        <<"application/vnd.lotus-freelance"/utf8>> ->
            [<<"pre"/utf8>>];

        <<"application/vnd.lotus-notes"/utf8>> ->
            [<<"nsf"/utf8>>];

        <<"application/vnd.lotus-organizer"/utf8>> ->
            [<<"org"/utf8>>];

        <<"application/vnd.lotus-screencam"/utf8>> ->
            [<<"scm"/utf8>>];

        <<"application/vnd.lotus-wordpro"/utf8>> ->
            [<<"lwp"/utf8>>];

        <<"application/vnd.macports.portpkg"/utf8>> ->
            [<<"portpkg"/utf8>>];

        <<"application/vnd.mcd"/utf8>> ->
            [<<"mcd"/utf8>>];

        <<"application/vnd.medcalcdata"/utf8>> ->
            [<<"mc1"/utf8>>];

        <<"application/vnd.mediastation.cdkey"/utf8>> ->
            [<<"cdkey"/utf8>>];

        <<"application/vnd.mfer"/utf8>> ->
            [<<"mwf"/utf8>>];

        <<"application/vnd.mfmp"/utf8>> ->
            [<<"mfm"/utf8>>];

        <<"application/vnd.micrografx.flo"/utf8>> ->
            [<<"flo"/utf8>>];

        <<"application/vnd.micrografx.igx"/utf8>> ->
            [<<"igx"/utf8>>];

        <<"application/vnd.mif"/utf8>> ->
            [<<"mif"/utf8>>];

        <<"application/vnd.mobius.daf"/utf8>> ->
            [<<"daf"/utf8>>];

        <<"application/vnd.mobius.dis"/utf8>> ->
            [<<"dis"/utf8>>];

        <<"application/vnd.mobius.mbk"/utf8>> ->
            [<<"mbk"/utf8>>];

        <<"application/vnd.mobius.mqy"/utf8>> ->
            [<<"mqy"/utf8>>];

        <<"application/vnd.mobius.msl"/utf8>> ->
            [<<"msl"/utf8>>];

        <<"application/vnd.mobius.plc"/utf8>> ->
            [<<"plc"/utf8>>];

        <<"application/vnd.mobius.txf"/utf8>> ->
            [<<"txf"/utf8>>];

        <<"application/vnd.mophun.application"/utf8>> ->
            [<<"mpn"/utf8>>];

        <<"application/vnd.mophun.certificate"/utf8>> ->
            [<<"mpc"/utf8>>];

        <<"application/vnd.mozilla.xul+xml"/utf8>> ->
            [<<"xul"/utf8>>];

        <<"application/vnd.ms-artgalry"/utf8>> ->
            [<<"cil"/utf8>>];

        <<"application/vnd.ms-cab-compressed"/utf8>> ->
            [<<"cab"/utf8>>];

        <<"application/vnd.ms-excel"/utf8>> ->
            [<<"xls"/utf8>>,
                <<"xlm"/utf8>>,
                <<"xla"/utf8>>,
                <<"xlc"/utf8>>,
                <<"xlt"/utf8>>,
                <<"xlw"/utf8>>];

        <<"application/vnd.ms-excel.addin.macroenabled.12"/utf8>> ->
            [<<"xlam"/utf8>>];

        <<"application/vnd.ms-excel.sheet.binary.macroenabled.12"/utf8>> ->
            [<<"xlsb"/utf8>>];

        <<"application/vnd.ms-excel.sheet.macroenabled.12"/utf8>> ->
            [<<"xlsm"/utf8>>];

        <<"application/vnd.ms-excel.template.macroenabled.12"/utf8>> ->
            [<<"xltm"/utf8>>];

        <<"application/vnd.ms-fontobject"/utf8>> ->
            [<<"eot"/utf8>>];

        <<"application/vnd.ms-htmlhelp"/utf8>> ->
            [<<"chm"/utf8>>];

        <<"application/vnd.ms-ims"/utf8>> ->
            [<<"ims"/utf8>>];

        <<"application/vnd.ms-lrm"/utf8>> ->
            [<<"lrm"/utf8>>];

        <<"application/vnd.ms-officetheme"/utf8>> ->
            [<<"thmx"/utf8>>];

        <<"application/vnd.ms-pki.seccat"/utf8>> ->
            [<<"cat"/utf8>>];

        <<"application/vnd.ms-pki.stl"/utf8>> ->
            [<<"stl"/utf8>>];

        <<"application/vnd.ms-powerpoint"/utf8>> ->
            [<<"ppt"/utf8>>, <<"pps"/utf8>>, <<"pot"/utf8>>];

        <<"application/vnd.ms-powerpoint.addin.macroenabled.12"/utf8>> ->
            [<<"ppam"/utf8>>];

        <<"application/vnd.ms-powerpoint.presentation.macroenabled.12"/utf8>> ->
            [<<"pptm"/utf8>>];

        <<"application/vnd.ms-powerpoint.slide.macroenabled.12"/utf8>> ->
            [<<"sldm"/utf8>>];

        <<"application/vnd.ms-powerpoint.slideshow.macroenabled.12"/utf8>> ->
            [<<"ppsm"/utf8>>];

        <<"application/vnd.ms-powerpoint.template.macroenabled.12"/utf8>> ->
            [<<"potm"/utf8>>];

        <<"application/vnd.ms-project"/utf8>> ->
            [<<"mpp"/utf8>>, <<"mpt"/utf8>>];

        <<"application/vnd.ms-word.document.macroenabled.12"/utf8>> ->
            [<<"docm"/utf8>>];

        <<"application/vnd.ms-word.template.macroenabled.12"/utf8>> ->
            [<<"dotm"/utf8>>];

        <<"application/vnd.ms-works"/utf8>> ->
            [<<"wps"/utf8>>, <<"wks"/utf8>>, <<"wcm"/utf8>>, <<"wdb"/utf8>>];

        <<"application/vnd.ms-wpl"/utf8>> ->
            [<<"wpl"/utf8>>];

        <<"application/vnd.ms-xpsdocument"/utf8>> ->
            [<<"xps"/utf8>>];

        <<"application/vnd.mseq"/utf8>> ->
            [<<"mseq"/utf8>>];

        <<"application/vnd.musician"/utf8>> ->
            [<<"mus"/utf8>>];

        <<"application/vnd.muvee.style"/utf8>> ->
            [<<"msty"/utf8>>];

        <<"application/vnd.mynfc"/utf8>> ->
            [<<"taglet"/utf8>>];

        <<"application/vnd.neurolanguage.nlu"/utf8>> ->
            [<<"nlu"/utf8>>];

        <<"application/vnd.nitf"/utf8>> ->
            [<<"ntf"/utf8>>, <<"nitf"/utf8>>];

        <<"application/vnd.noblenet-directory"/utf8>> ->
            [<<"nnd"/utf8>>];

        <<"application/vnd.noblenet-sealer"/utf8>> ->
            [<<"nns"/utf8>>];

        <<"application/vnd.noblenet-web"/utf8>> ->
            [<<"nnw"/utf8>>];

        <<"application/vnd.nokia.n-gage.data"/utf8>> ->
            [<<"ngdat"/utf8>>];

        <<"application/vnd.nokia.n-gage.symbian.install"/utf8>> ->
            [<<"n-gage"/utf8>>];

        <<"application/vnd.nokia.radio-preset"/utf8>> ->
            [<<"rpst"/utf8>>];

        <<"application/vnd.nokia.radio-presets"/utf8>> ->
            [<<"rpss"/utf8>>];

        <<"application/vnd.novadigm.edm"/utf8>> ->
            [<<"edm"/utf8>>];

        <<"application/vnd.novadigm.edx"/utf8>> ->
            [<<"edx"/utf8>>];

        <<"application/vnd.novadigm.ext"/utf8>> ->
            [<<"ext"/utf8>>];

        <<"application/vnd.oasis.opendocument.chart"/utf8>> ->
            [<<"odc"/utf8>>];

        <<"application/vnd.oasis.opendocument.chart-template"/utf8>> ->
            [<<"otc"/utf8>>];

        <<"application/vnd.oasis.opendocument.database"/utf8>> ->
            [<<"odb"/utf8>>];

        <<"application/vnd.oasis.opendocument.formula"/utf8>> ->
            [<<"odf"/utf8>>];

        <<"application/vnd.oasis.opendocument.formula-template"/utf8>> ->
            [<<"odft"/utf8>>];

        <<"application/vnd.oasis.opendocument.graphics"/utf8>> ->
            [<<"odg"/utf8>>];

        <<"application/vnd.oasis.opendocument.graphics-template"/utf8>> ->
            [<<"otg"/utf8>>];

        <<"application/vnd.oasis.opendocument.image"/utf8>> ->
            [<<"odi"/utf8>>];

        <<"application/vnd.oasis.opendocument.image-template"/utf8>> ->
            [<<"oti"/utf8>>];

        <<"application/vnd.oasis.opendocument.presentation"/utf8>> ->
            [<<"odp"/utf8>>];

        <<"application/vnd.oasis.opendocument.presentation-template"/utf8>> ->
            [<<"otp"/utf8>>];

        <<"application/vnd.oasis.opendocument.spreadsheet"/utf8>> ->
            [<<"ods"/utf8>>];

        <<"application/vnd.oasis.opendocument.spreadsheet-template"/utf8>> ->
            [<<"ots"/utf8>>];

        <<"application/vnd.oasis.opendocument.text"/utf8>> ->
            [<<"odt"/utf8>>];

        <<"application/vnd.oasis.opendocument.text-master"/utf8>> ->
            [<<"odm"/utf8>>];

        <<"application/vnd.oasis.opendocument.text-template"/utf8>> ->
            [<<"ott"/utf8>>];

        <<"application/vnd.oasis.opendocument.text-web"/utf8>> ->
            [<<"oth"/utf8>>];

        <<"application/vnd.olpc-sugar"/utf8>> ->
            [<<"xo"/utf8>>];

        <<"application/vnd.oma.dd2+xml"/utf8>> ->
            [<<"dd2"/utf8>>];

        <<"application/vnd.openofficeorg.extension"/utf8>> ->
            [<<"oxt"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.presentationml.presentation"/utf8>> ->
            [<<"pptx"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.presentationml.slide"/utf8>> ->
            [<<"sldx"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.presentationml.slideshow"/utf8>> ->
            [<<"ppsx"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.presentationml.template"/utf8>> ->
            [<<"potx"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"/utf8>> ->
            [<<"xlsx"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.spreadsheetml.template"/utf8>> ->
            [<<"xltx"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document"/utf8>> ->
            [<<"docx"/utf8>>];

        <<"application/vnd.openxmlformats-officedocument.wordprocessingml.template"/utf8>> ->
            [<<"dotx"/utf8>>];

        <<"application/vnd.osgeo.mapguide.package"/utf8>> ->
            [<<"mgp"/utf8>>];

        <<"application/vnd.osgi.dp"/utf8>> ->
            [<<"dp"/utf8>>];

        <<"application/vnd.osgi.subsystem"/utf8>> ->
            [<<"esa"/utf8>>];

        <<"application/vnd.palm"/utf8>> ->
            [<<"pdb"/utf8>>, <<"pqa"/utf8>>, <<"oprc"/utf8>>];

        <<"application/vnd.pawaafile"/utf8>> ->
            [<<"paw"/utf8>>];

        <<"application/vnd.pg.format"/utf8>> ->
            [<<"str"/utf8>>];

        <<"application/vnd.pg.osasli"/utf8>> ->
            [<<"ei6"/utf8>>];

        <<"application/vnd.picsel"/utf8>> ->
            [<<"efif"/utf8>>];

        <<"application/vnd.pmi.widget"/utf8>> ->
            [<<"wg"/utf8>>];

        <<"application/vnd.pocketlearn"/utf8>> ->
            [<<"plf"/utf8>>];

        <<"application/vnd.powerbuilder6"/utf8>> ->
            [<<"pbd"/utf8>>];

        <<"application/vnd.previewsystems.box"/utf8>> ->
            [<<"box"/utf8>>];

        <<"application/vnd.proteus.magazine"/utf8>> ->
            [<<"mgz"/utf8>>];

        <<"application/vnd.publishare-delta-tree"/utf8>> ->
            [<<"qps"/utf8>>];

        <<"application/vnd.pvi.ptid1"/utf8>> ->
            [<<"ptid"/utf8>>];

        <<"application/vnd.quark.quarkxpress"/utf8>> ->
            [<<"qxd"/utf8>>,
                <<"qxt"/utf8>>,
                <<"qwd"/utf8>>,
                <<"qwt"/utf8>>,
                <<"qxl"/utf8>>,
                <<"qxb"/utf8>>];

        <<"application/vnd.realvnc.bed"/utf8>> ->
            [<<"bed"/utf8>>];

        <<"application/vnd.recordare.musicxml"/utf8>> ->
            [<<"mxl"/utf8>>];

        <<"application/vnd.recordare.musicxml+xml"/utf8>> ->
            [<<"musicxml"/utf8>>];

        <<"application/vnd.rig.cryptonote"/utf8>> ->
            [<<"cryptonote"/utf8>>];

        <<"application/vnd.rim.cod"/utf8>> ->
            [<<"cod"/utf8>>];

        <<"application/vnd.rn-realmedia"/utf8>> ->
            [<<"rm"/utf8>>];

        <<"application/vnd.rn-realmedia-vbr"/utf8>> ->
            [<<"rmvb"/utf8>>];

        <<"application/vnd.route66.link66+xml"/utf8>> ->
            [<<"link66"/utf8>>];

        <<"application/vnd.sailingtracker.track"/utf8>> ->
            [<<"st"/utf8>>];

        <<"application/vnd.seemail"/utf8>> ->
            [<<"see"/utf8>>];

        <<"application/vnd.sema"/utf8>> ->
            [<<"sema"/utf8>>];

        <<"application/vnd.semd"/utf8>> ->
            [<<"semd"/utf8>>];

        <<"application/vnd.semf"/utf8>> ->
            [<<"semf"/utf8>>];

        <<"application/vnd.shana.informed.formdata"/utf8>> ->
            [<<"ifm"/utf8>>];

        <<"application/vnd.shana.informed.formtemplate"/utf8>> ->
            [<<"itp"/utf8>>];

        <<"application/vnd.shana.informed.interchange"/utf8>> ->
            [<<"iif"/utf8>>];

        <<"application/vnd.shana.informed.package"/utf8>> ->
            [<<"ipk"/utf8>>];

        <<"application/vnd.simtech-mindmapper"/utf8>> ->
            [<<"twd"/utf8>>, <<"twds"/utf8>>];

        <<"application/vnd.smaf"/utf8>> ->
            [<<"mmf"/utf8>>];

        <<"application/vnd.smart.teacher"/utf8>> ->
            [<<"teacher"/utf8>>];

        <<"application/vnd.solent.sdkm+xml"/utf8>> ->
            [<<"sdkm"/utf8>>, <<"sdkd"/utf8>>];

        <<"application/vnd.spotfire.dxp"/utf8>> ->
            [<<"dxp"/utf8>>];

        <<"application/vnd.spotfire.sfs"/utf8>> ->
            [<<"sfs"/utf8>>];

        <<"application/vnd.stardivision.calc"/utf8>> ->
            [<<"sdc"/utf8>>];

        <<"application/vnd.stardivision.draw"/utf8>> ->
            [<<"sda"/utf8>>];

        <<"application/vnd.stardivision.impress"/utf8>> ->
            [<<"sdd"/utf8>>];

        <<"application/vnd.stardivision.math"/utf8>> ->
            [<<"smf"/utf8>>];

        <<"application/vnd.stardivision.writer"/utf8>> ->
            [<<"sdw"/utf8>>, <<"vor"/utf8>>];

        <<"application/vnd.stardivision.writer-global"/utf8>> ->
            [<<"sgl"/utf8>>];

        <<"application/vnd.stepmania.package"/utf8>> ->
            [<<"smzip"/utf8>>];

        <<"application/vnd.stepmania.stepchart"/utf8>> ->
            [<<"sm"/utf8>>];

        <<"application/vnd.sun.xml.calc"/utf8>> ->
            [<<"sxc"/utf8>>];

        <<"application/vnd.sun.xml.calc.template"/utf8>> ->
            [<<"stc"/utf8>>];

        <<"application/vnd.sun.xml.draw"/utf8>> ->
            [<<"sxd"/utf8>>];

        <<"application/vnd.sun.xml.draw.template"/utf8>> ->
            [<<"std"/utf8>>];

        <<"application/vnd.sun.xml.impress"/utf8>> ->
            [<<"sxi"/utf8>>];

        <<"application/vnd.sun.xml.impress.template"/utf8>> ->
            [<<"sti"/utf8>>];

        <<"application/vnd.sun.xml.math"/utf8>> ->
            [<<"sxm"/utf8>>];

        <<"application/vnd.sun.xml.writer"/utf8>> ->
            [<<"sxw"/utf8>>];

        <<"application/vnd.sun.xml.writer.global"/utf8>> ->
            [<<"sxg"/utf8>>];

        <<"application/vnd.sun.xml.writer.template"/utf8>> ->
            [<<"stw"/utf8>>];

        <<"application/vnd.sus-calendar"/utf8>> ->
            [<<"sus"/utf8>>, <<"susp"/utf8>>];

        <<"application/vnd.svd"/utf8>> ->
            [<<"svd"/utf8>>];

        <<"application/vnd.symbian.install"/utf8>> ->
            [<<"sis"/utf8>>, <<"sisx"/utf8>>];

        <<"application/vnd.syncml+xml"/utf8>> ->
            [<<"xsm"/utf8>>];

        <<"application/vnd.syncml.dm+wbxml"/utf8>> ->
            [<<"bdm"/utf8>>];

        <<"application/vnd.syncml.dm+xml"/utf8>> ->
            [<<"xdm"/utf8>>];

        <<"application/vnd.tao.intent-module-archive"/utf8>> ->
            [<<"tao"/utf8>>];

        <<"application/vnd.tcpdump.pcap"/utf8>> ->
            [<<"pcap"/utf8>>, <<"cap"/utf8>>, <<"dmp"/utf8>>];

        <<"application/vnd.tmobile-livetv"/utf8>> ->
            [<<"tmo"/utf8>>];

        <<"application/vnd.trid.tpt"/utf8>> ->
            [<<"tpt"/utf8>>];

        <<"application/vnd.triscape.mxs"/utf8>> ->
            [<<"mxs"/utf8>>];

        <<"application/vnd.trueapp"/utf8>> ->
            [<<"tra"/utf8>>];

        <<"application/vnd.ufdl"/utf8>> ->
            [<<"ufd"/utf8>>, <<"ufdl"/utf8>>];

        <<"application/vnd.uiq.theme"/utf8>> ->
            [<<"utz"/utf8>>];

        <<"application/vnd.umajin"/utf8>> ->
            [<<"umj"/utf8>>];

        <<"application/vnd.unity"/utf8>> ->
            [<<"unityweb"/utf8>>];

        <<"application/vnd.uoml+xml"/utf8>> ->
            [<<"uoml"/utf8>>];

        <<"application/vnd.vcx"/utf8>> ->
            [<<"vcx"/utf8>>];

        <<"application/vnd.visio"/utf8>> ->
            [<<"vsd"/utf8>>, <<"vst"/utf8>>, <<"vss"/utf8>>, <<"vsw"/utf8>>];

        <<"application/vnd.visionary"/utf8>> ->
            [<<"vis"/utf8>>];

        <<"application/vnd.vsf"/utf8>> ->
            [<<"vsf"/utf8>>];

        <<"application/vnd.wap.wbxml"/utf8>> ->
            [<<"wbxml"/utf8>>];

        <<"application/vnd.wap.wmlc"/utf8>> ->
            [<<"wmlc"/utf8>>];

        <<"application/vnd.wap.wmlscriptc"/utf8>> ->
            [<<"wmlsc"/utf8>>];

        <<"application/vnd.webturbo"/utf8>> ->
            [<<"wtb"/utf8>>];

        <<"application/vnd.wolfram.player"/utf8>> ->
            [<<"nbp"/utf8>>];

        <<"application/vnd.wordperfect"/utf8>> ->
            [<<"wpd"/utf8>>];

        <<"application/vnd.wqd"/utf8>> ->
            [<<"wqd"/utf8>>];

        <<"application/vnd.wt.stf"/utf8>> ->
            [<<"stf"/utf8>>];

        <<"application/vnd.xara"/utf8>> ->
            [<<"xar"/utf8>>];

        <<"application/vnd.xfdl"/utf8>> ->
            [<<"xfdl"/utf8>>];

        <<"application/vnd.yamaha.hv-dic"/utf8>> ->
            [<<"hvd"/utf8>>];

        <<"application/vnd.yamaha.hv-script"/utf8>> ->
            [<<"hvs"/utf8>>];

        <<"application/vnd.yamaha.hv-voice"/utf8>> ->
            [<<"hvp"/utf8>>];

        <<"application/vnd.yamaha.openscoreformat"/utf8>> ->
            [<<"osf"/utf8>>];

        <<"application/vnd.yamaha.openscoreformat.osfpvg+xml"/utf8>> ->
            [<<"osfpvg"/utf8>>];

        <<"application/vnd.yamaha.smaf-audio"/utf8>> ->
            [<<"saf"/utf8>>];

        <<"application/vnd.yamaha.smaf-phrase"/utf8>> ->
            [<<"spf"/utf8>>];

        <<"application/vnd.yellowriver-custom-menu"/utf8>> ->
            [<<"cmp"/utf8>>];

        <<"application/vnd.zul"/utf8>> ->
            [<<"zir"/utf8>>, <<"zirz"/utf8>>];

        <<"application/vnd.zzazz.deck+xml"/utf8>> ->
            [<<"zaz"/utf8>>];

        <<"application/voicexml+xml"/utf8>> ->
            [<<"vxml"/utf8>>];

        <<"application/wasm"/utf8>> ->
            [<<"wasm"/utf8>>];

        <<"application/widget"/utf8>> ->
            [<<"wgt"/utf8>>];

        <<"application/winhlp"/utf8>> ->
            [<<"hlp"/utf8>>];

        <<"application/wsdl+xml"/utf8>> ->
            [<<"wsdl"/utf8>>];

        <<"application/wspolicy+xml"/utf8>> ->
            [<<"wspolicy"/utf8>>];

        <<"application/x-7z-compressed"/utf8>> ->
            [<<"7z"/utf8>>];

        <<"application/x-abiword"/utf8>> ->
            [<<"abw"/utf8>>];

        <<"application/x-ace-compressed"/utf8>> ->
            [<<"ace"/utf8>>];

        <<"application/x-apple-diskimage"/utf8>> ->
            [<<"dmg"/utf8>>];

        <<"application/x-authorware-bin"/utf8>> ->
            [<<"aab"/utf8>>, <<"x32"/utf8>>, <<"u32"/utf8>>, <<"vox"/utf8>>];

        <<"application/x-authorware-map"/utf8>> ->
            [<<"aam"/utf8>>];

        <<"application/x-authorware-seg"/utf8>> ->
            [<<"aas"/utf8>>];

        <<"application/x-bcpio"/utf8>> ->
            [<<"bcpio"/utf8>>];

        <<"application/x-bittorrent"/utf8>> ->
            [<<"torrent"/utf8>>];

        <<"application/x-blorb"/utf8>> ->
            [<<"blb"/utf8>>, <<"blorb"/utf8>>];

        <<"application/x-bzip"/utf8>> ->
            [<<"bz"/utf8>>];

        <<"application/x-bzip2"/utf8>> ->
            [<<"bz2"/utf8>>, <<"boz"/utf8>>];

        <<"application/x-cbr"/utf8>> ->
            [<<"cbr"/utf8>>,
                <<"cba"/utf8>>,
                <<"cbt"/utf8>>,
                <<"cbz"/utf8>>,
                <<"cb7"/utf8>>];

        <<"application/x-cdlink"/utf8>> ->
            [<<"vcd"/utf8>>];

        <<"application/x-cfs-compressed"/utf8>> ->
            [<<"cfs"/utf8>>];

        <<"application/x-chat"/utf8>> ->
            [<<"chat"/utf8>>];

        <<"application/x-chess-pgn"/utf8>> ->
            [<<"pgn"/utf8>>];

        <<"application/x-conference"/utf8>> ->
            [<<"nsc"/utf8>>];

        <<"application/x-cpio"/utf8>> ->
            [<<"cpio"/utf8>>];

        <<"application/x-csh"/utf8>> ->
            [<<"csh"/utf8>>];

        <<"application/x-debian-package"/utf8>> ->
            [<<"deb"/utf8>>, <<"udeb"/utf8>>];

        <<"application/x-dgc-compressed"/utf8>> ->
            [<<"dgc"/utf8>>];

        <<"application/x-director"/utf8>> ->
            [<<"dir"/utf8>>,
                <<"dcr"/utf8>>,
                <<"dxr"/utf8>>,
                <<"cst"/utf8>>,
                <<"cct"/utf8>>,
                <<"cxt"/utf8>>,
                <<"w3d"/utf8>>,
                <<"fgd"/utf8>>,
                <<"swa"/utf8>>];

        <<"application/x-doom"/utf8>> ->
            [<<"wad"/utf8>>];

        <<"application/x-dtbncx+xml"/utf8>> ->
            [<<"ncx"/utf8>>];

        <<"application/x-dtbook+xml"/utf8>> ->
            [<<"dtb"/utf8>>];

        <<"application/x-dtbresource+xml"/utf8>> ->
            [<<"res"/utf8>>];

        <<"application/x-dvi"/utf8>> ->
            [<<"dvi"/utf8>>];

        <<"application/x-envoy"/utf8>> ->
            [<<"evy"/utf8>>];

        <<"application/x-eva"/utf8>> ->
            [<<"eva"/utf8>>];

        <<"application/x-font-bdf"/utf8>> ->
            [<<"bdf"/utf8>>];

        <<"application/x-font-ghostscript"/utf8>> ->
            [<<"gsf"/utf8>>];

        <<"application/x-font-linux-psf"/utf8>> ->
            [<<"psf"/utf8>>];

        <<"application/x-font-pcf"/utf8>> ->
            [<<"pcf"/utf8>>];

        <<"application/x-font-snf"/utf8>> ->
            [<<"snf"/utf8>>];

        <<"application/x-font-type1"/utf8>> ->
            [<<"pfa"/utf8>>, <<"pfb"/utf8>>, <<"pfm"/utf8>>, <<"afm"/utf8>>];

        <<"application/x-freearc"/utf8>> ->
            [<<"arc"/utf8>>];

        <<"application/x-futuresplash"/utf8>> ->
            [<<"spl"/utf8>>];

        <<"application/x-gca-compressed"/utf8>> ->
            [<<"gca"/utf8>>];

        <<"application/x-glulx"/utf8>> ->
            [<<"ulx"/utf8>>];

        <<"application/x-gnumeric"/utf8>> ->
            [<<"gnumeric"/utf8>>];

        <<"application/x-gramps-xml"/utf8>> ->
            [<<"gramps"/utf8>>];

        <<"application/x-gtar"/utf8>> ->
            [<<"gtar"/utf8>>];

        <<"application/x-hdf"/utf8>> ->
            [<<"hdf"/utf8>>];

        <<"application/x-install-instructions"/utf8>> ->
            [<<"install"/utf8>>];

        <<"application/x-iso9660-image"/utf8>> ->
            [<<"iso"/utf8>>];

        <<"application/x-java-jnlp-file"/utf8>> ->
            [<<"jnlp"/utf8>>];

        <<"application/x-latex"/utf8>> ->
            [<<"latex"/utf8>>];

        <<"application/x-lzh-compressed"/utf8>> ->
            [<<"lzh"/utf8>>, <<"lha"/utf8>>];

        <<"application/x-mie"/utf8>> ->
            [<<"mie"/utf8>>];

        <<"application/x-mobipocket-ebook"/utf8>> ->
            [<<"prc"/utf8>>, <<"mobi"/utf8>>];

        <<"application/x-ms-application"/utf8>> ->
            [<<"application"/utf8>>];

        <<"application/x-ms-shortcut"/utf8>> ->
            [<<"lnk"/utf8>>];

        <<"application/x-ms-wmd"/utf8>> ->
            [<<"wmd"/utf8>>];

        <<"application/x-ms-wmz"/utf8>> ->
            [<<"wmz"/utf8>>];

        <<"application/x-ms-xbap"/utf8>> ->
            [<<"xbap"/utf8>>];

        <<"application/x-msaccess"/utf8>> ->
            [<<"mdb"/utf8>>];

        <<"application/x-msbinder"/utf8>> ->
            [<<"obd"/utf8>>];

        <<"application/x-mscardfile"/utf8>> ->
            [<<"crd"/utf8>>];

        <<"application/x-msclip"/utf8>> ->
            [<<"clp"/utf8>>];

        <<"application/x-msdownload"/utf8>> ->
            [<<"exe"/utf8>>,
                <<"dll"/utf8>>,
                <<"com"/utf8>>,
                <<"bat"/utf8>>,
                <<"msi"/utf8>>];

        <<"application/x-msmediaview"/utf8>> ->
            [<<"mvb"/utf8>>, <<"m13"/utf8>>, <<"m14"/utf8>>];

        <<"application/x-msmetafile"/utf8>> ->
            [<<"wmf"/utf8>>, <<"wmz"/utf8>>, <<"emf"/utf8>>, <<"emz"/utf8>>];

        <<"application/x-msmoney"/utf8>> ->
            [<<"mny"/utf8>>];

        <<"application/x-mspublisher"/utf8>> ->
            [<<"pub"/utf8>>];

        <<"application/x-msschedule"/utf8>> ->
            [<<"scd"/utf8>>];

        <<"application/x-msterminal"/utf8>> ->
            [<<"trm"/utf8>>];

        <<"application/x-mswrite"/utf8>> ->
            [<<"wri"/utf8>>];

        <<"application/x-netcdf"/utf8>> ->
            [<<"nc"/utf8>>, <<"cdf"/utf8>>];

        <<"application/x-nzb"/utf8>> ->
            [<<"nzb"/utf8>>];

        <<"application/x-pkcs12"/utf8>> ->
            [<<"p12"/utf8>>, <<"pfx"/utf8>>];

        <<"application/x-pkcs7-certificates"/utf8>> ->
            [<<"p7b"/utf8>>, <<"spc"/utf8>>];

        <<"application/x-pkcs7-certreqresp"/utf8>> ->
            [<<"p7r"/utf8>>];

        <<"application/x-rar-compressed"/utf8>> ->
            [<<"rar"/utf8>>];

        <<"application/x-research-info-systems"/utf8>> ->
            [<<"ris"/utf8>>];

        <<"application/x-sh"/utf8>> ->
            [<<"sh"/utf8>>];

        <<"application/x-shar"/utf8>> ->
            [<<"shar"/utf8>>];

        <<"application/x-shockwave-flash"/utf8>> ->
            [<<"swf"/utf8>>];

        <<"application/x-silverlight-app"/utf8>> ->
            [<<"xap"/utf8>>];

        <<"application/x-sql"/utf8>> ->
            [<<"sql"/utf8>>];

        <<"application/x-stuffit"/utf8>> ->
            [<<"sit"/utf8>>];

        <<"application/x-stuffitx"/utf8>> ->
            [<<"sitx"/utf8>>];

        <<"application/x-subrip"/utf8>> ->
            [<<"srt"/utf8>>];

        <<"application/x-sv4cpio"/utf8>> ->
            [<<"sv4cpio"/utf8>>];

        <<"application/x-sv4crc"/utf8>> ->
            [<<"sv4crc"/utf8>>];

        <<"application/x-t3vm-image"/utf8>> ->
            [<<"t3"/utf8>>];

        <<"application/x-tads"/utf8>> ->
            [<<"gam"/utf8>>];

        <<"application/x-tar"/utf8>> ->
            [<<"tar"/utf8>>];

        <<"application/x-tcl"/utf8>> ->
            [<<"tcl"/utf8>>];

        <<"application/x-tex"/utf8>> ->
            [<<"tex"/utf8>>];

        <<"application/x-tex-tfm"/utf8>> ->
            [<<"tfm"/utf8>>];

        <<"application/x-texinfo"/utf8>> ->
            [<<"texinfo"/utf8>>, <<"texi"/utf8>>];

        <<"application/x-tgif"/utf8>> ->
            [<<"obj"/utf8>>];

        <<"application/x-ustar"/utf8>> ->
            [<<"ustar"/utf8>>];

        <<"application/x-wais-source"/utf8>> ->
            [<<"src"/utf8>>];

        <<"application/x-x509-ca-cert"/utf8>> ->
            [<<"der"/utf8>>, <<"crt"/utf8>>];

        <<"application/x-xfig"/utf8>> ->
            [<<"fig"/utf8>>];

        <<"application/x-xliff+xml"/utf8>> ->
            [<<"xlf"/utf8>>];

        <<"application/x-xpinstall"/utf8>> ->
            [<<"xpi"/utf8>>];

        <<"application/x-xz"/utf8>> ->
            [<<"xz"/utf8>>];

        <<"application/x-zmachine"/utf8>> ->
            [<<"z1"/utf8>>,
                <<"z2"/utf8>>,
                <<"z3"/utf8>>,
                <<"z4"/utf8>>,
                <<"z5"/utf8>>,
                <<"z6"/utf8>>,
                <<"z7"/utf8>>,
                <<"z8"/utf8>>];

        <<"application/xaml+xml"/utf8>> ->
            [<<"xaml"/utf8>>];

        <<"application/xcap-diff+xml"/utf8>> ->
            [<<"xdf"/utf8>>];

        <<"application/xenc+xml"/utf8>> ->
            [<<"xenc"/utf8>>];

        <<"application/xhtml+xml"/utf8>> ->
            [<<"xhtml"/utf8>>, <<"xht"/utf8>>];

        <<"application/xml"/utf8>> ->
            [<<"xml"/utf8>>, <<"xsl"/utf8>>];

        <<"application/xml-dtd"/utf8>> ->
            [<<"dtd"/utf8>>];

        <<"application/xop+xml"/utf8>> ->
            [<<"xop"/utf8>>];

        <<"application/xproc+xml"/utf8>> ->
            [<<"xpl"/utf8>>];

        <<"application/xslt+xml"/utf8>> ->
            [<<"xslt"/utf8>>];

        <<"application/xspf+xml"/utf8>> ->
            [<<"xspf"/utf8>>];

        <<"application/xv+xml"/utf8>> ->
            [<<"mxml"/utf8>>, <<"xhvml"/utf8>>, <<"xvml"/utf8>>, <<"xvm"/utf8>>];

        <<"application/yang"/utf8>> ->
            [<<"yang"/utf8>>];

        <<"application/yin+xml"/utf8>> ->
            [<<"yin"/utf8>>];

        <<"application/zip"/utf8>> ->
            [<<"zip"/utf8>>];

        <<"audio/adpcm"/utf8>> ->
            [<<"adp"/utf8>>];

        <<"audio/basic"/utf8>> ->
            [<<"au"/utf8>>, <<"snd"/utf8>>];

        <<"audio/midi"/utf8>> ->
            [<<"mid"/utf8>>, <<"midi"/utf8>>, <<"kar"/utf8>>, <<"rmi"/utf8>>];

        <<"audio/mp4"/utf8>> ->
            [<<"m4a"/utf8>>, <<"mp4a"/utf8>>];

        <<"audio/mpeg"/utf8>> ->
            [<<"mpga"/utf8>>,
                <<"mp2"/utf8>>,
                <<"mp2a"/utf8>>,
                <<"mp3"/utf8>>,
                <<"m2a"/utf8>>,
                <<"m3a"/utf8>>];

        <<"audio/ogg"/utf8>> ->
            [<<"oga"/utf8>>, <<"ogg"/utf8>>, <<"spx"/utf8>>, <<"opus"/utf8>>];

        <<"audio/s3m"/utf8>> ->
            [<<"s3m"/utf8>>];

        <<"audio/silk"/utf8>> ->
            [<<"sil"/utf8>>];

        <<"audio/vnd.dece.audio"/utf8>> ->
            [<<"uva"/utf8>>, <<"uvva"/utf8>>];

        <<"audio/vnd.digital-winds"/utf8>> ->
            [<<"eol"/utf8>>];

        <<"audio/vnd.dra"/utf8>> ->
            [<<"dra"/utf8>>];

        <<"audio/vnd.dts"/utf8>> ->
            [<<"dts"/utf8>>];

        <<"audio/vnd.dts.hd"/utf8>> ->
            [<<"dtshd"/utf8>>];

        <<"audio/vnd.lucent.voice"/utf8>> ->
            [<<"lvp"/utf8>>];

        <<"audio/vnd.ms-playready.media.pya"/utf8>> ->
            [<<"pya"/utf8>>];

        <<"audio/vnd.nuera.ecelp4800"/utf8>> ->
            [<<"ecelp4800"/utf8>>];

        <<"audio/vnd.nuera.ecelp7470"/utf8>> ->
            [<<"ecelp7470"/utf8>>];

        <<"audio/vnd.nuera.ecelp9600"/utf8>> ->
            [<<"ecelp9600"/utf8>>];

        <<"audio/vnd.rip"/utf8>> ->
            [<<"rip"/utf8>>];

        <<"audio/webm"/utf8>> ->
            [<<"weba"/utf8>>];

        <<"audio/x-aac"/utf8>> ->
            [<<"aac"/utf8>>];

        <<"audio/x-aiff"/utf8>> ->
            [<<"aif"/utf8>>, <<"aiff"/utf8>>, <<"aifc"/utf8>>];

        <<"audio/x-caf"/utf8>> ->
            [<<"caf"/utf8>>];

        <<"audio/x-flac"/utf8>> ->
            [<<"flac"/utf8>>];

        <<"audio/x-matroska"/utf8>> ->
            [<<"mka"/utf8>>];

        <<"audio/x-mpegurl"/utf8>> ->
            [<<"m3u"/utf8>>];

        <<"audio/x-ms-wax"/utf8>> ->
            [<<"wax"/utf8>>];

        <<"audio/x-ms-wma"/utf8>> ->
            [<<"wma"/utf8>>];

        <<"audio/x-pn-realaudio"/utf8>> ->
            [<<"ram"/utf8>>, <<"ra"/utf8>>];

        <<"audio/x-pn-realaudio-plugin"/utf8>> ->
            [<<"rmp"/utf8>>];

        <<"audio/x-wav"/utf8>> ->
            [<<"wav"/utf8>>];

        <<"audio/xm"/utf8>> ->
            [<<"xm"/utf8>>];

        <<"chemical/x-cdx"/utf8>> ->
            [<<"cdx"/utf8>>];

        <<"chemical/x-cif"/utf8>> ->
            [<<"cif"/utf8>>];

        <<"chemical/x-cmdf"/utf8>> ->
            [<<"cmdf"/utf8>>];

        <<"chemical/x-cml"/utf8>> ->
            [<<"cml"/utf8>>];

        <<"chemical/x-csml"/utf8>> ->
            [<<"csml"/utf8>>];

        <<"chemical/x-xyz"/utf8>> ->
            [<<"xyz"/utf8>>];

        <<"font/collection"/utf8>> ->
            [<<"ttc"/utf8>>];

        <<"font/otf"/utf8>> ->
            [<<"otf"/utf8>>];

        <<"font/ttf"/utf8>> ->
            [<<"ttf"/utf8>>];

        <<"font/woff"/utf8>> ->
            [<<"woff"/utf8>>];

        <<"font/woff2"/utf8>> ->
            [<<"woff2"/utf8>>];

        <<"image/avif"/utf8>> ->
            [<<"avif"/utf8>>];

        <<"image/bmp"/utf8>> ->
            [<<"bmp"/utf8>>];

        <<"image/cgm"/utf8>> ->
            [<<"cgm"/utf8>>];

        <<"image/g3fax"/utf8>> ->
            [<<"g3"/utf8>>];

        <<"image/gif"/utf8>> ->
            [<<"gif"/utf8>>];

        <<"image/ief"/utf8>> ->
            [<<"ief"/utf8>>];

        <<"image/jpeg"/utf8>> ->
            [<<"jpeg"/utf8>>, <<"jpg"/utf8>>, <<"jpe"/utf8>>];

        <<"image/jxl"/utf8>> ->
            [<<"jxl"/utf8>>];

        <<"image/ktx"/utf8>> ->
            [<<"ktx"/utf8>>];

        <<"image/png"/utf8>> ->
            [<<"png"/utf8>>];

        <<"image/prs.btif"/utf8>> ->
            [<<"btif"/utf8>>];

        <<"image/sgi"/utf8>> ->
            [<<"sgi"/utf8>>];

        <<"image/svg+xml"/utf8>> ->
            [<<"svg"/utf8>>, <<"svgz"/utf8>>];

        <<"image/tiff"/utf8>> ->
            [<<"tiff"/utf8>>, <<"tif"/utf8>>];

        <<"image/vnd.adobe.photoshop"/utf8>> ->
            [<<"psd"/utf8>>];

        <<"image/vnd.dece.graphic"/utf8>> ->
            [<<"uvi"/utf8>>, <<"uvvi"/utf8>>, <<"uvg"/utf8>>, <<"uvvg"/utf8>>];

        <<"image/vnd.djvu"/utf8>> ->
            [<<"djvu"/utf8>>, <<"djv"/utf8>>];

        <<"image/vnd.dvb.subtitle"/utf8>> ->
            [<<"sub"/utf8>>];

        <<"image/vnd.dwg"/utf8>> ->
            [<<"dwg"/utf8>>];

        <<"image/vnd.dxf"/utf8>> ->
            [<<"dxf"/utf8>>];

        <<"image/vnd.fastbidsheet"/utf8>> ->
            [<<"fbs"/utf8>>];

        <<"image/vnd.fpx"/utf8>> ->
            [<<"fpx"/utf8>>];

        <<"image/vnd.fst"/utf8>> ->
            [<<"fst"/utf8>>];

        <<"image/vnd.fujixerox.edmics-mmr"/utf8>> ->
            [<<"mmr"/utf8>>];

        <<"image/vnd.fujixerox.edmics-rlc"/utf8>> ->
            [<<"rlc"/utf8>>];

        <<"image/vnd.ms-modi"/utf8>> ->
            [<<"mdi"/utf8>>];

        <<"image/vnd.ms-photo"/utf8>> ->
            [<<"wdp"/utf8>>];

        <<"image/vnd.net-fpx"/utf8>> ->
            [<<"npx"/utf8>>];

        <<"image/vnd.wap.wbmp"/utf8>> ->
            [<<"wbmp"/utf8>>];

        <<"image/vnd.xiff"/utf8>> ->
            [<<"xif"/utf8>>];

        <<"image/webp"/utf8>> ->
            [<<"webp"/utf8>>];

        <<"image/x-3ds"/utf8>> ->
            [<<"3ds"/utf8>>];

        <<"image/x-cmu-raster"/utf8>> ->
            [<<"ras"/utf8>>];

        <<"image/x-cmx"/utf8>> ->
            [<<"cmx"/utf8>>];

        <<"image/x-freehand"/utf8>> ->
            [<<"fh"/utf8>>,
                <<"fhc"/utf8>>,
                <<"fh4"/utf8>>,
                <<"fh5"/utf8>>,
                <<"fh7"/utf8>>];

        <<"image/x-icon"/utf8>> ->
            [<<"ico"/utf8>>];

        <<"image/x-mrsid-image"/utf8>> ->
            [<<"sid"/utf8>>];

        <<"image/x-pcx"/utf8>> ->
            [<<"pcx"/utf8>>];

        <<"image/x-pict"/utf8>> ->
            [<<"pic"/utf8>>, <<"pct"/utf8>>];

        <<"image/x-portable-anymap"/utf8>> ->
            [<<"pnm"/utf8>>];

        <<"image/x-portable-bitmap"/utf8>> ->
            [<<"pbm"/utf8>>];

        <<"image/x-portable-graymap"/utf8>> ->
            [<<"pgm"/utf8>>];

        <<"image/x-portable-pixmap"/utf8>> ->
            [<<"ppm"/utf8>>];

        <<"image/x-rgb"/utf8>> ->
            [<<"rgb"/utf8>>];

        <<"image/x-tga"/utf8>> ->
            [<<"tga"/utf8>>];

        <<"image/x-xbitmap"/utf8>> ->
            [<<"xbm"/utf8>>];

        <<"image/x-xpixmap"/utf8>> ->
            [<<"xpm"/utf8>>];

        <<"image/x-xwindowdump"/utf8>> ->
            [<<"xwd"/utf8>>];

        <<"message/rfc822"/utf8>> ->
            [<<"eml"/utf8>>, <<"mime"/utf8>>];

        <<"model/iges"/utf8>> ->
            [<<"igs"/utf8>>, <<"iges"/utf8>>];

        <<"model/mesh"/utf8>> ->
            [<<"msh"/utf8>>, <<"mesh"/utf8>>, <<"silo"/utf8>>];

        <<"model/vnd.collada+xml"/utf8>> ->
            [<<"dae"/utf8>>];

        <<"model/vnd.dwf"/utf8>> ->
            [<<"dwf"/utf8>>];

        <<"model/vnd.gdl"/utf8>> ->
            [<<"gdl"/utf8>>];

        <<"model/vnd.gtw"/utf8>> ->
            [<<"gtw"/utf8>>];

        <<"model/vnd.vtu"/utf8>> ->
            [<<"vtu"/utf8>>];

        <<"model/vrml"/utf8>> ->
            [<<"wrl"/utf8>>, <<"vrml"/utf8>>];

        <<"model/x3d+binary"/utf8>> ->
            [<<"x3db"/utf8>>, <<"x3dbz"/utf8>>];

        <<"model/x3d+vrml"/utf8>> ->
            [<<"x3dv"/utf8>>, <<"x3dvz"/utf8>>];

        <<"model/x3d+xml"/utf8>> ->
            [<<"x3d"/utf8>>, <<"x3dz"/utf8>>];

        <<"text/cache-manifest"/utf8>> ->
            [<<"appcache"/utf8>>];

        <<"text/calendar"/utf8>> ->
            [<<"ics"/utf8>>, <<"ifb"/utf8>>];

        <<"text/css"/utf8>> ->
            [<<"css"/utf8>>];

        <<"text/csv"/utf8>> ->
            [<<"csv"/utf8>>];

        <<"text/html"/utf8>> ->
            [<<"html"/utf8>>, <<"htm"/utf8>>];

        <<"text/javascript"/utf8>> ->
            [<<"js"/utf8>>, <<"mjs"/utf8>>];

        <<"text/n3"/utf8>> ->
            [<<"n3"/utf8>>];

        <<"text/plain"/utf8>> ->
            [<<"txt"/utf8>>,
                <<"text"/utf8>>,
                <<"conf"/utf8>>,
                <<"def"/utf8>>,
                <<"list"/utf8>>,
                <<"log"/utf8>>,
                <<"in"/utf8>>];

        <<"text/prs.lines.tag"/utf8>> ->
            [<<"dsc"/utf8>>];

        <<"text/richtext"/utf8>> ->
            [<<"rtx"/utf8>>];

        <<"text/sgml"/utf8>> ->
            [<<"sgml"/utf8>>, <<"sgm"/utf8>>];

        <<"text/tab-separated-values"/utf8>> ->
            [<<"tsv"/utf8>>];

        <<"text/troff"/utf8>> ->
            [<<"t"/utf8>>,
                <<"tr"/utf8>>,
                <<"roff"/utf8>>,
                <<"man"/utf8>>,
                <<"me"/utf8>>,
                <<"ms"/utf8>>];

        <<"text/turtle"/utf8>> ->
            [<<"ttl"/utf8>>];

        <<"text/uri-list"/utf8>> ->
            [<<"uri"/utf8>>, <<"uris"/utf8>>, <<"urls"/utf8>>];

        <<"text/vcard"/utf8>> ->
            [<<"vcard"/utf8>>];

        <<"text/vnd.curl"/utf8>> ->
            [<<"curl"/utf8>>];

        <<"text/vnd.curl.dcurl"/utf8>> ->
            [<<"dcurl"/utf8>>];

        <<"text/vnd.curl.mcurl"/utf8>> ->
            [<<"mcurl"/utf8>>];

        <<"text/vnd.curl.scurl"/utf8>> ->
            [<<"scurl"/utf8>>];

        <<"text/vnd.dvb.subtitle"/utf8>> ->
            [<<"sub"/utf8>>];

        <<"text/vnd.fly"/utf8>> ->
            [<<"fly"/utf8>>];

        <<"text/vnd.fmi.flexstor"/utf8>> ->
            [<<"flx"/utf8>>];

        <<"text/vnd.graphviz"/utf8>> ->
            [<<"gv"/utf8>>];

        <<"text/vnd.in3d.3dml"/utf8>> ->
            [<<"3dml"/utf8>>];

        <<"text/vnd.in3d.spot"/utf8>> ->
            [<<"spot"/utf8>>];

        <<"text/vnd.sun.j2me.app-descriptor"/utf8>> ->
            [<<"jad"/utf8>>];

        <<"text/vnd.wap.wml"/utf8>> ->
            [<<"wml"/utf8>>];

        <<"text/vnd.wap.wmlscript"/utf8>> ->
            [<<"wmls"/utf8>>];

        <<"text/x-asm"/utf8>> ->
            [<<"s"/utf8>>, <<"asm"/utf8>>];

        <<"text/x-c"/utf8>> ->
            [<<"c"/utf8>>,
                <<"cc"/utf8>>,
                <<"cxx"/utf8>>,
                <<"cpp"/utf8>>,
                <<"h"/utf8>>,
                <<"hh"/utf8>>,
                <<"dic"/utf8>>];

        <<"text/x-fortran"/utf8>> ->
            [<<"f"/utf8>>, <<"for"/utf8>>, <<"f77"/utf8>>, <<"f90"/utf8>>];

        <<"text/x-java-source"/utf8>> ->
            [<<"java"/utf8>>];

        <<"text/x-nfo"/utf8>> ->
            [<<"nfo"/utf8>>];

        <<"text/x-opml"/utf8>> ->
            [<<"opml"/utf8>>];

        <<"text/x-pascal"/utf8>> ->
            [<<"p"/utf8>>, <<"pas"/utf8>>];

        <<"text/x-setext"/utf8>> ->
            [<<"etx"/utf8>>];

        <<"text/x-sfv"/utf8>> ->
            [<<"sfv"/utf8>>];

        <<"text/x-uuencode"/utf8>> ->
            [<<"uu"/utf8>>];

        <<"text/x-vcalendar"/utf8>> ->
            [<<"vcs"/utf8>>];

        <<"text/x-vcard"/utf8>> ->
            [<<"vcf"/utf8>>];

        <<"video/3gpp"/utf8>> ->
            [<<"3gp"/utf8>>];

        <<"video/3gpp2"/utf8>> ->
            [<<"3g2"/utf8>>];

        <<"video/h261"/utf8>> ->
            [<<"h261"/utf8>>];

        <<"video/h263"/utf8>> ->
            [<<"h263"/utf8>>];

        <<"video/h264"/utf8>> ->
            [<<"h264"/utf8>>];

        <<"video/jpeg"/utf8>> ->
            [<<"jpgv"/utf8>>];

        <<"video/jpm"/utf8>> ->
            [<<"jpm"/utf8>>, <<"jpgm"/utf8>>];

        <<"video/mj2"/utf8>> ->
            [<<"mj2"/utf8>>, <<"mjp2"/utf8>>];

        <<"video/mp2t"/utf8>> ->
            [<<"ts"/utf8>>, <<"m2t"/utf8>>, <<"m2ts"/utf8>>, <<"mts"/utf8>>];

        <<"video/mp4"/utf8>> ->
            [<<"mp4"/utf8>>, <<"mp4v"/utf8>>, <<"mpg4"/utf8>>];

        <<"video/mpeg"/utf8>> ->
            [<<"mpeg"/utf8>>,
                <<"mpg"/utf8>>,
                <<"mpe"/utf8>>,
                <<"m1v"/utf8>>,
                <<"m2v"/utf8>>];

        <<"video/ogg"/utf8>> ->
            [<<"ogv"/utf8>>];

        <<"video/quicktime"/utf8>> ->
            [<<"qt"/utf8>>, <<"mov"/utf8>>];

        <<"video/vnd.dece.hd"/utf8>> ->
            [<<"uvh"/utf8>>, <<"uvvh"/utf8>>];

        <<"video/vnd.dece.mobile"/utf8>> ->
            [<<"uvm"/utf8>>, <<"uvvm"/utf8>>];

        <<"video/vnd.dece.pd"/utf8>> ->
            [<<"uvp"/utf8>>, <<"uvvp"/utf8>>];

        <<"video/vnd.dece.sd"/utf8>> ->
            [<<"uvs"/utf8>>, <<"uvvs"/utf8>>];

        <<"video/vnd.dece.video"/utf8>> ->
            [<<"uvv"/utf8>>, <<"uvvv"/utf8>>];

        <<"video/vnd.dvb.file"/utf8>> ->
            [<<"dvb"/utf8>>];

        <<"video/vnd.fvt"/utf8>> ->
            [<<"fvt"/utf8>>];

        <<"video/vnd.mpegurl"/utf8>> ->
            [<<"mxu"/utf8>>, <<"m4u"/utf8>>];

        <<"video/vnd.ms-playready.media.pyv"/utf8>> ->
            [<<"pyv"/utf8>>];

        <<"video/vnd.uvvu.mp4"/utf8>> ->
            [<<"uvu"/utf8>>, <<"uvvu"/utf8>>];

        <<"video/vnd.vivo"/utf8>> ->
            [<<"viv"/utf8>>];

        <<"video/webm"/utf8>> ->
            [<<"webm"/utf8>>];

        <<"video/x-f4v"/utf8>> ->
            [<<"f4v"/utf8>>];

        <<"video/x-fli"/utf8>> ->
            [<<"fli"/utf8>>];

        <<"video/x-flv"/utf8>> ->
            [<<"flv"/utf8>>];

        <<"video/x-m4v"/utf8>> ->
            [<<"m4v"/utf8>>];

        <<"video/x-matroska"/utf8>> ->
            [<<"mkv"/utf8>>, <<"mk3d"/utf8>>, <<"mks"/utf8>>];

        <<"video/x-mng"/utf8>> ->
            [<<"mng"/utf8>>];

        <<"video/x-ms-asf"/utf8>> ->
            [<<"asf"/utf8>>, <<"asx"/utf8>>];

        <<"video/x-ms-vob"/utf8>> ->
            [<<"vob"/utf8>>];

        <<"video/x-ms-wm"/utf8>> ->
            [<<"wm"/utf8>>];

        <<"video/x-ms-wmv"/utf8>> ->
            [<<"wmv"/utf8>>];

        <<"video/x-ms-wmx"/utf8>> ->
            [<<"wmx"/utf8>>];

        <<"video/x-ms-wvx"/utf8>> ->
            [<<"wvx"/utf8>>];

        <<"video/x-msvideo"/utf8>> ->
            [<<"avi"/utf8>>];

        <<"video/x-sgi-movie"/utf8>> ->
            [<<"movie"/utf8>>];

        <<"video/x-smv"/utf8>> ->
            [<<"smv"/utf8>>];

        <<"x-conference/x-cooltalk"/utf8>> ->
            [<<"ice"/utf8>>];

        _ ->
            []
    end.
