import { toList } from "./gleam.mjs";

export function extension_to_mime_type(extension) {
  if (extension === "123") {
    return "application/vnd.lotus-1-2-3";
  } else if (extension === "3dml") {
    return "text/vnd.in3d.3dml";
  } else if (extension === "3ds") {
    return "image/x-3ds";
  } else if (extension === "3g2") {
    return "video/3gpp2";
  } else if (extension === "3gp") {
    return "video/3gpp";
  } else if (extension === "7z") {
    return "application/x-7z-compressed";
  } else if (extension === "aab") {
    return "application/x-authorware-bin";
  } else if (extension === "aac") {
    return "audio/x-aac";
  } else if (extension === "aam") {
    return "application/x-authorware-map";
  } else if (extension === "aas") {
    return "application/x-authorware-seg";
  } else if (extension === "abw") {
    return "application/x-abiword";
  } else if (extension === "ac") {
    return "application/pkix-attr-cert";
  } else if (extension === "acc") {
    return "application/vnd.americandynamics.acc";
  } else if (extension === "ace") {
    return "application/x-ace-compressed";
  } else if (extension === "acu") {
    return "application/vnd.acucobol";
  } else if (extension === "acutc") {
    return "application/vnd.acucorp";
  } else if (extension === "adp") {
    return "audio/adpcm";
  } else if (extension === "aep") {
    return "application/vnd.audiograph";
  } else if (extension === "afm") {
    return "application/x-font-type1";
  } else if (extension === "afp") {
    return "application/vnd.ibm.modcap";
  } else if (extension === "ahead") {
    return "application/vnd.ahead.space";
  } else if (extension === "ai") {
    return "application/postscript";
  } else if (extension === "aif") {
    return "audio/x-aiff";
  } else if (extension === "aifc") {
    return "audio/x-aiff";
  } else if (extension === "aiff") {
    return "audio/x-aiff";
  } else if (extension === "air") {
    return "application/vnd.adobe.air-application-installer-package+zip";
  } else if (extension === "ait") {
    return "application/vnd.dvb.ait";
  } else if (extension === "ami") {
    return "application/vnd.amiga.ami";
  } else if (extension === "apk") {
    return "application/vnd.android.package-archive";
  } else if (extension === "appcache") {
    return "text/cache-manifest";
  } else if (extension === "application") {
    return "application/x-ms-application";
  } else if (extension === "apr") {
    return "application/vnd.lotus-approach";
  } else if (extension === "arc") {
    return "application/x-freearc";
  } else if (extension === "asc") {
    return "application/pgp-signature";
  } else if (extension === "asf") {
    return "video/x-ms-asf";
  } else if (extension === "asm") {
    return "text/x-asm";
  } else if (extension === "aso") {
    return "application/vnd.accpac.simply.aso";
  } else if (extension === "asx") {
    return "video/x-ms-asf";
  } else if (extension === "atc") {
    return "application/vnd.acucorp";
  } else if (extension === "atom") {
    return "application/atom+xml";
  } else if (extension === "atomcat") {
    return "application/atomcat+xml";
  } else if (extension === "atomsvc") {
    return "application/atomsvc+xml";
  } else if (extension === "atx") {
    return "application/vnd.antix.game-component";
  } else if (extension === "au") {
    return "audio/basic";
  } else if (extension === "avi") {
    return "video/x-msvideo";
  } else if (extension === "avif") {
    return "image/avif";
  } else if (extension === "aw") {
    return "application/applixware";
  } else if (extension === "azf") {
    return "application/vnd.airzip.filesecure.azf";
  } else if (extension === "azs") {
    return "application/vnd.airzip.filesecure.azs";
  } else if (extension === "azw") {
    return "application/vnd.amazon.ebook";
  } else if (extension === "bat") {
    return "application/x-msdownload";
  } else if (extension === "bcpio") {
    return "application/x-bcpio";
  } else if (extension === "bdf") {
    return "application/x-font-bdf";
  } else if (extension === "bdm") {
    return "application/vnd.syncml.dm+wbxml";
  } else if (extension === "bed") {
    return "application/vnd.realvnc.bed";
  } else if (extension === "bh2") {
    return "application/vnd.fujitsu.oasysprs";
  } else if (extension === "bin") {
    return "application/octet-stream";
  } else if (extension === "blb") {
    return "application/x-blorb";
  } else if (extension === "blorb") {
    return "application/x-blorb";
  } else if (extension === "bmi") {
    return "application/vnd.bmi";
  } else if (extension === "bmp") {
    return "image/bmp";
  } else if (extension === "book") {
    return "application/vnd.framemaker";
  } else if (extension === "box") {
    return "application/vnd.previewsystems.box";
  } else if (extension === "boz") {
    return "application/x-bzip2";
  } else if (extension === "bpk") {
    return "application/octet-stream";
  } else if (extension === "btif") {
    return "image/prs.btif";
  } else if (extension === "bz") {
    return "application/x-bzip";
  } else if (extension === "bz2") {
    return "application/x-bzip2";
  } else if (extension === "c") {
    return "text/x-c";
  } else if (extension === "c11amc") {
    return "application/vnd.cluetrust.cartomobile-config";
  } else if (extension === "c11amz") {
    return "application/vnd.cluetrust.cartomobile-config-pkg";
  } else if (extension === "c4d") {
    return "application/vnd.clonk.c4group";
  } else if (extension === "c4f") {
    return "application/vnd.clonk.c4group";
  } else if (extension === "c4g") {
    return "application/vnd.clonk.c4group";
  } else if (extension === "c4p") {
    return "application/vnd.clonk.c4group";
  } else if (extension === "c4u") {
    return "application/vnd.clonk.c4group";
  } else if (extension === "cab") {
    return "application/vnd.ms-cab-compressed";
  } else if (extension === "caf") {
    return "audio/x-caf";
  } else if (extension === "cap") {
    return "application/vnd.tcpdump.pcap";
  } else if (extension === "car") {
    return "application/vnd.curl.car";
  } else if (extension === "cat") {
    return "application/vnd.ms-pki.seccat";
  } else if (extension === "cb7") {
    return "application/x-cbr";
  } else if (extension === "cba") {
    return "application/x-cbr";
  } else if (extension === "cbr") {
    return "application/x-cbr";
  } else if (extension === "cbt") {
    return "application/x-cbr";
  } else if (extension === "cbz") {
    return "application/x-cbr";
  } else if (extension === "cc") {
    return "text/x-c";
  } else if (extension === "cct") {
    return "application/x-director";
  } else if (extension === "ccxml") {
    return "application/ccxml+xml";
  } else if (extension === "cdbcmsg") {
    return "application/vnd.contact.cmsg";
  } else if (extension === "cdf") {
    return "application/x-netcdf";
  } else if (extension === "cdkey") {
    return "application/vnd.mediastation.cdkey";
  } else if (extension === "cdmia") {
    return "application/cdmi-capability";
  } else if (extension === "cdmic") {
    return "application/cdmi-container";
  } else if (extension === "cdmid") {
    return "application/cdmi-domain";
  } else if (extension === "cdmio") {
    return "application/cdmi-object";
  } else if (extension === "cdmiq") {
    return "application/cdmi-queue";
  } else if (extension === "cdx") {
    return "chemical/x-cdx";
  } else if (extension === "cdxml") {
    return "application/vnd.chemdraw+xml";
  } else if (extension === "cdy") {
    return "application/vnd.cinderella";
  } else if (extension === "cer") {
    return "application/pkix-cert";
  } else if (extension === "cfs") {
    return "application/x-cfs-compressed";
  } else if (extension === "cgm") {
    return "image/cgm";
  } else if (extension === "chat") {
    return "application/x-chat";
  } else if (extension === "chm") {
    return "application/vnd.ms-htmlhelp";
  } else if (extension === "chrt") {
    return "application/vnd.kde.kchart";
  } else if (extension === "cif") {
    return "chemical/x-cif";
  } else if (extension === "cii") {
    return "application/vnd.anser-web-certificate-issue-initiation";
  } else if (extension === "cil") {
    return "application/vnd.ms-artgalry";
  } else if (extension === "cla") {
    return "application/vnd.claymore";
  } else if (extension === "class") {
    return "application/java-vm";
  } else if (extension === "clkk") {
    return "application/vnd.crick.clicker.keyboard";
  } else if (extension === "clkp") {
    return "application/vnd.crick.clicker.palette";
  } else if (extension === "clkt") {
    return "application/vnd.crick.clicker.template";
  } else if (extension === "clkw") {
    return "application/vnd.crick.clicker.wordbank";
  } else if (extension === "clkx") {
    return "application/vnd.crick.clicker";
  } else if (extension === "clp") {
    return "application/x-msclip";
  } else if (extension === "cmc") {
    return "application/vnd.cosmocaller";
  } else if (extension === "cmdf") {
    return "chemical/x-cmdf";
  } else if (extension === "cml") {
    return "chemical/x-cml";
  } else if (extension === "cmp") {
    return "application/vnd.yellowriver-custom-menu";
  } else if (extension === "cmx") {
    return "image/x-cmx";
  } else if (extension === "cod") {
    return "application/vnd.rim.cod";
  } else if (extension === "com") {
    return "application/x-msdownload";
  } else if (extension === "conf") {
    return "text/plain";
  } else if (extension === "cpio") {
    return "application/x-cpio";
  } else if (extension === "cpp") {
    return "text/x-c";
  } else if (extension === "cpt") {
    return "application/mac-compactpro";
  } else if (extension === "crd") {
    return "application/x-mscardfile";
  } else if (extension === "crl") {
    return "application/pkix-crl";
  } else if (extension === "crt") {
    return "application/x-x509-ca-cert";
  } else if (extension === "cryptonote") {
    return "application/vnd.rig.cryptonote";
  } else if (extension === "csh") {
    return "application/x-csh";
  } else if (extension === "csml") {
    return "chemical/x-csml";
  } else if (extension === "csp") {
    return "application/vnd.commonspace";
  } else if (extension === "css") {
    return "text/css";
  } else if (extension === "cst") {
    return "application/x-director";
  } else if (extension === "csv") {
    return "text/csv";
  } else if (extension === "cu") {
    return "application/cu-seeme";
  } else if (extension === "curl") {
    return "text/vnd.curl";
  } else if (extension === "cww") {
    return "application/prs.cww";
  } else if (extension === "cxt") {
    return "application/x-director";
  } else if (extension === "cxx") {
    return "text/x-c";
  } else if (extension === "dae") {
    return "model/vnd.collada+xml";
  } else if (extension === "daf") {
    return "application/vnd.mobius.daf";
  } else if (extension === "dart") {
    return "application/vnd.dart";
  } else if (extension === "dataless") {
    return "application/vnd.fdsn.seed";
  } else if (extension === "davmount") {
    return "application/davmount+xml";
  } else if (extension === "dbk") {
    return "application/docbook+xml";
  } else if (extension === "dcr") {
    return "application/x-director";
  } else if (extension === "dcurl") {
    return "text/vnd.curl.dcurl";
  } else if (extension === "dd2") {
    return "application/vnd.oma.dd2+xml";
  } else if (extension === "ddd") {
    return "application/vnd.fujixerox.ddd";
  } else if (extension === "deb") {
    return "application/x-debian-package";
  } else if (extension === "def") {
    return "text/plain";
  } else if (extension === "deploy") {
    return "application/octet-stream";
  } else if (extension === "der") {
    return "application/x-x509-ca-cert";
  } else if (extension === "dfac") {
    return "application/vnd.dreamfactory";
  } else if (extension === "dgc") {
    return "application/x-dgc-compressed";
  } else if (extension === "dic") {
    return "text/x-c";
  } else if (extension === "dir") {
    return "application/x-director";
  } else if (extension === "dis") {
    return "application/vnd.mobius.dis";
  } else if (extension === "dist") {
    return "application/octet-stream";
  } else if (extension === "distz") {
    return "application/octet-stream";
  } else if (extension === "djv") {
    return "image/vnd.djvu";
  } else if (extension === "djvu") {
    return "image/vnd.djvu";
  } else if (extension === "dll") {
    return "application/x-msdownload";
  } else if (extension === "dmg") {
    return "application/x-apple-diskimage";
  } else if (extension === "dmp") {
    return "application/vnd.tcpdump.pcap";
  } else if (extension === "dms") {
    return "application/octet-stream";
  } else if (extension === "dna") {
    return "application/vnd.dna";
  } else if (extension === "doc") {
    return "application/msword";
  } else if (extension === "docm") {
    return "application/vnd.ms-word.document.macroenabled.12";
  } else if (extension === "docx") {
    return "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
  } else if (extension === "dot") {
    return "application/msword";
  } else if (extension === "dotm") {
    return "application/vnd.ms-word.template.macroenabled.12";
  } else if (extension === "dotx") {
    return "application/vnd.openxmlformats-officedocument.wordprocessingml.template";
  } else if (extension === "dp") {
    return "application/vnd.osgi.dp";
  } else if (extension === "dpg") {
    return "application/vnd.dpgraph";
  } else if (extension === "dra") {
    return "audio/vnd.dra";
  } else if (extension === "dsc") {
    return "text/prs.lines.tag";
  } else if (extension === "dssc") {
    return "application/dssc+der";
  } else if (extension === "dtb") {
    return "application/x-dtbook+xml";
  } else if (extension === "dtd") {
    return "application/xml-dtd";
  } else if (extension === "dts") {
    return "audio/vnd.dts";
  } else if (extension === "dtshd") {
    return "audio/vnd.dts.hd";
  } else if (extension === "dump") {
    return "application/octet-stream";
  } else if (extension === "dvb") {
    return "video/vnd.dvb.file";
  } else if (extension === "dvi") {
    return "application/x-dvi";
  } else if (extension === "dwf") {
    return "model/vnd.dwf";
  } else if (extension === "dwg") {
    return "image/vnd.dwg";
  } else if (extension === "dxf") {
    return "image/vnd.dxf";
  } else if (extension === "dxp") {
    return "application/vnd.spotfire.dxp";
  } else if (extension === "dxr") {
    return "application/x-director";
  } else if (extension === "ecelp4800") {
    return "audio/vnd.nuera.ecelp4800";
  } else if (extension === "ecelp7470") {
    return "audio/vnd.nuera.ecelp7470";
  } else if (extension === "ecelp9600") {
    return "audio/vnd.nuera.ecelp9600";
  } else if (extension === "ecma") {
    return "application/ecmascript";
  } else if (extension === "edm") {
    return "application/vnd.novadigm.edm";
  } else if (extension === "edx") {
    return "application/vnd.novadigm.edx";
  } else if (extension === "efif") {
    return "application/vnd.picsel";
  } else if (extension === "ei6") {
    return "application/vnd.pg.osasli";
  } else if (extension === "elc") {
    return "application/octet-stream";
  } else if (extension === "emf") {
    return "application/x-msmetafile";
  } else if (extension === "eml") {
    return "message/rfc822";
  } else if (extension === "emma") {
    return "application/emma+xml";
  } else if (extension === "emz") {
    return "application/x-msmetafile";
  } else if (extension === "eol") {
    return "audio/vnd.digital-winds";
  } else if (extension === "eot") {
    return "application/vnd.ms-fontobject";
  } else if (extension === "eps") {
    return "application/postscript";
  } else if (extension === "epub") {
    return "application/epub+zip";
  } else if (extension === "es3") {
    return "application/vnd.eszigno3+xml";
  } else if (extension === "esa") {
    return "application/vnd.osgi.subsystem";
  } else if (extension === "esf") {
    return "application/vnd.epson.esf";
  } else if (extension === "et3") {
    return "application/vnd.eszigno3+xml";
  } else if (extension === "etx") {
    return "text/x-setext";
  } else if (extension === "eva") {
    return "application/x-eva";
  } else if (extension === "evy") {
    return "application/x-envoy";
  } else if (extension === "exe") {
    return "application/x-msdownload";
  } else if (extension === "exi") {
    return "application/exi";
  } else if (extension === "ext") {
    return "application/vnd.novadigm.ext";
  } else if (extension === "ez") {
    return "application/andrew-inset";
  } else if (extension === "ez2") {
    return "application/vnd.ezpix-album";
  } else if (extension === "ez3") {
    return "application/vnd.ezpix-package";
  } else if (extension === "f") {
    return "text/x-fortran";
  } else if (extension === "f4v") {
    return "video/x-f4v";
  } else if (extension === "f77") {
    return "text/x-fortran";
  } else if (extension === "f90") {
    return "text/x-fortran";
  } else if (extension === "fbs") {
    return "image/vnd.fastbidsheet";
  } else if (extension === "fcdt") {
    return "application/vnd.adobe.formscentral.fcdt";
  } else if (extension === "fcs") {
    return "application/vnd.isac.fcs";
  } else if (extension === "fdf") {
    return "application/vnd.fdf";
  } else if (extension === "fe_launch") {
    return "application/vnd.denovo.fcselayout-link";
  } else if (extension === "fg5") {
    return "application/vnd.fujitsu.oasysgp";
  } else if (extension === "fgd") {
    return "application/x-director";
  } else if (extension === "fh") {
    return "image/x-freehand";
  } else if (extension === "fh4") {
    return "image/x-freehand";
  } else if (extension === "fh5") {
    return "image/x-freehand";
  } else if (extension === "fh7") {
    return "image/x-freehand";
  } else if (extension === "fhc") {
    return "image/x-freehand";
  } else if (extension === "fig") {
    return "application/x-xfig";
  } else if (extension === "flac") {
    return "audio/x-flac";
  } else if (extension === "fli") {
    return "video/x-fli";
  } else if (extension === "flo") {
    return "application/vnd.micrografx.flo";
  } else if (extension === "flv") {
    return "video/x-flv";
  } else if (extension === "flw") {
    return "application/vnd.kde.kivio";
  } else if (extension === "flx") {
    return "text/vnd.fmi.flexstor";
  } else if (extension === "fly") {
    return "text/vnd.fly";
  } else if (extension === "fm") {
    return "application/vnd.framemaker";
  } else if (extension === "fnc") {
    return "application/vnd.frogans.fnc";
  } else if (extension === "for") {
    return "text/x-fortran";
  } else if (extension === "fpx") {
    return "image/vnd.fpx";
  } else if (extension === "frame") {
    return "application/vnd.framemaker";
  } else if (extension === "fsc") {
    return "application/vnd.fsc.weblaunch";
  } else if (extension === "fst") {
    return "image/vnd.fst";
  } else if (extension === "ftc") {
    return "application/vnd.fluxtime.clip";
  } else if (extension === "fti") {
    return "application/vnd.anser-web-funds-transfer-initiation";
  } else if (extension === "fvt") {
    return "video/vnd.fvt";
  } else if (extension === "fxp") {
    return "application/vnd.adobe.fxp";
  } else if (extension === "fxpl") {
    return "application/vnd.adobe.fxp";
  } else if (extension === "fzs") {
    return "application/vnd.fuzzysheet";
  } else if (extension === "g2w") {
    return "application/vnd.geoplan";
  } else if (extension === "g3") {
    return "image/g3fax";
  } else if (extension === "g3w") {
    return "application/vnd.geospace";
  } else if (extension === "gac") {
    return "application/vnd.groove-account";
  } else if (extension === "gam") {
    return "application/x-tads";
  } else if (extension === "gbr") {
    return "application/rpki-ghostbusters";
  } else if (extension === "gca") {
    return "application/x-gca-compressed";
  } else if (extension === "gdl") {
    return "model/vnd.gdl";
  } else if (extension === "geo") {
    return "application/vnd.dynageo";
  } else if (extension === "gex") {
    return "application/vnd.geometry-explorer";
  } else if (extension === "ggb") {
    return "application/vnd.geogebra.file";
  } else if (extension === "ggs") {
    return "application/vnd.geogebra.slides";
  } else if (extension === "ggt") {
    return "application/vnd.geogebra.tool";
  } else if (extension === "ghf") {
    return "application/vnd.groove-help";
  } else if (extension === "gif") {
    return "image/gif";
  } else if (extension === "gim") {
    return "application/vnd.groove-identity-message";
  } else if (extension === "gml") {
    return "application/gml+xml";
  } else if (extension === "gmx") {
    return "application/vnd.gmx";
  } else if (extension === "gnumeric") {
    return "application/x-gnumeric";
  } else if (extension === "gph") {
    return "application/vnd.flographit";
  } else if (extension === "gpx") {
    return "application/gpx+xml";
  } else if (extension === "gqf") {
    return "application/vnd.grafeq";
  } else if (extension === "gqs") {
    return "application/vnd.grafeq";
  } else if (extension === "gram") {
    return "application/srgs";
  } else if (extension === "gramps") {
    return "application/x-gramps-xml";
  } else if (extension === "gre") {
    return "application/vnd.geometry-explorer";
  } else if (extension === "grv") {
    return "application/vnd.groove-injector";
  } else if (extension === "grxml") {
    return "application/srgs+xml";
  } else if (extension === "gsf") {
    return "application/x-font-ghostscript";
  } else if (extension === "gtar") {
    return "application/x-gtar";
  } else if (extension === "gtm") {
    return "application/vnd.groove-tool-message";
  } else if (extension === "gtw") {
    return "model/vnd.gtw";
  } else if (extension === "gv") {
    return "text/vnd.graphviz";
  } else if (extension === "gxf") {
    return "application/gxf";
  } else if (extension === "gxt") {
    return "application/vnd.geonext";
  } else if (extension === "h") {
    return "text/x-c";
  } else if (extension === "h261") {
    return "video/h261";
  } else if (extension === "h263") {
    return "video/h263";
  } else if (extension === "h264") {
    return "video/h264";
  } else if (extension === "hal") {
    return "application/vnd.hal+xml";
  } else if (extension === "hbci") {
    return "application/vnd.hbci";
  } else if (extension === "hdf") {
    return "application/x-hdf";
  } else if (extension === "hh") {
    return "text/x-c";
  } else if (extension === "hlp") {
    return "application/winhlp";
  } else if (extension === "hpgl") {
    return "application/vnd.hp-hpgl";
  } else if (extension === "hpid") {
    return "application/vnd.hp-hpid";
  } else if (extension === "hps") {
    return "application/vnd.hp-hps";
  } else if (extension === "hqx") {
    return "application/mac-binhex40";
  } else if (extension === "htke") {
    return "application/vnd.kenameaapp";
  } else if (extension === "htm") {
    return "text/html";
  } else if (extension === "html") {
    return "text/html";
  } else if (extension === "hvd") {
    return "application/vnd.yamaha.hv-dic";
  } else if (extension === "hvp") {
    return "application/vnd.yamaha.hv-voice";
  } else if (extension === "hvs") {
    return "application/vnd.yamaha.hv-script";
  } else if (extension === "i2g") {
    return "application/vnd.intergeo";
  } else if (extension === "icc") {
    return "application/vnd.iccprofile";
  } else if (extension === "ice") {
    return "x-conference/x-cooltalk";
  } else if (extension === "icm") {
    return "application/vnd.iccprofile";
  } else if (extension === "ico") {
    return "image/x-icon";
  } else if (extension === "ics") {
    return "text/calendar";
  } else if (extension === "ief") {
    return "image/ief";
  } else if (extension === "ifb") {
    return "text/calendar";
  } else if (extension === "ifm") {
    return "application/vnd.shana.informed.formdata";
  } else if (extension === "iges") {
    return "model/iges";
  } else if (extension === "igl") {
    return "application/vnd.igloader";
  } else if (extension === "igm") {
    return "application/vnd.insors.igm";
  } else if (extension === "igs") {
    return "model/iges";
  } else if (extension === "igx") {
    return "application/vnd.micrografx.igx";
  } else if (extension === "iif") {
    return "application/vnd.shana.informed.interchange";
  } else if (extension === "imp") {
    return "application/vnd.accpac.simply.imp";
  } else if (extension === "ims") {
    return "application/vnd.ms-ims";
  } else if (extension === "in") {
    return "text/plain";
  } else if (extension === "ink") {
    return "application/inkml+xml";
  } else if (extension === "inkml") {
    return "application/inkml+xml";
  } else if (extension === "install") {
    return "application/x-install-instructions";
  } else if (extension === "iota") {
    return "application/vnd.astraea-software.iota";
  } else if (extension === "ipfix") {
    return "application/ipfix";
  } else if (extension === "ipk") {
    return "application/vnd.shana.informed.package";
  } else if (extension === "irm") {
    return "application/vnd.ibm.rights-management";
  } else if (extension === "irp") {
    return "application/vnd.irepository.package+xml";
  } else if (extension === "iso") {
    return "application/x-iso9660-image";
  } else if (extension === "itp") {
    return "application/vnd.shana.informed.formtemplate";
  } else if (extension === "ivp") {
    return "application/vnd.immervision-ivp";
  } else if (extension === "ivu") {
    return "application/vnd.immervision-ivu";
  } else if (extension === "jad") {
    return "text/vnd.sun.j2me.app-descriptor";
  } else if (extension === "jam") {
    return "application/vnd.jam";
  } else if (extension === "jar") {
    return "application/java-archive";
  } else if (extension === "java") {
    return "text/x-java-source";
  } else if (extension === "jisp") {
    return "application/vnd.jisp";
  } else if (extension === "jlt") {
    return "application/vnd.hp-jlyt";
  } else if (extension === "jnlp") {
    return "application/x-java-jnlp-file";
  } else if (extension === "joda") {
    return "application/vnd.joost.joda-archive";
  } else if (extension === "jpe") {
    return "image/jpeg";
  } else if (extension === "jpeg") {
    return "image/jpeg";
  } else if (extension === "jpg") {
    return "image/jpeg";
  } else if (extension === "jpgm") {
    return "video/jpm";
  } else if (extension === "jpgv") {
    return "video/jpeg";
  } else if (extension === "jpm") {
    return "video/jpm";
  } else if (extension === "js") {
    return "text/javascript";
  } else if (extension === "json") {
    return "application/json";
  } else if (extension === "jsonml") {
    return "application/jsonml+json";
  } else if (extension === "jxl") {
    return "image/jxl";
  } else if (extension === "kar") {
    return "audio/midi";
  } else if (extension === "karbon") {
    return "application/vnd.kde.karbon";
  } else if (extension === "kfo") {
    return "application/vnd.kde.kformula";
  } else if (extension === "kia") {
    return "application/vnd.kidspiration";
  } else if (extension === "kml") {
    return "application/vnd.google-earth.kml+xml";
  } else if (extension === "kmz") {
    return "application/vnd.google-earth.kmz";
  } else if (extension === "kne") {
    return "application/vnd.kinar";
  } else if (extension === "knp") {
    return "application/vnd.kinar";
  } else if (extension === "kon") {
    return "application/vnd.kde.kontour";
  } else if (extension === "kpr") {
    return "application/vnd.kde.kpresenter";
  } else if (extension === "kpt") {
    return "application/vnd.kde.kpresenter";
  } else if (extension === "kpxx") {
    return "application/vnd.ds-keypoint";
  } else if (extension === "ksp") {
    return "application/vnd.kde.kspread";
  } else if (extension === "ktr") {
    return "application/vnd.kahootz";
  } else if (extension === "ktx") {
    return "image/ktx";
  } else if (extension === "ktz") {
    return "application/vnd.kahootz";
  } else if (extension === "kwd") {
    return "application/vnd.kde.kword";
  } else if (extension === "kwt") {
    return "application/vnd.kde.kword";
  } else if (extension === "lasxml") {
    return "application/vnd.las.las+xml";
  } else if (extension === "latex") {
    return "application/x-latex";
  } else if (extension === "lbd") {
    return "application/vnd.llamagraphics.life-balance.desktop";
  } else if (extension === "lbe") {
    return "application/vnd.llamagraphics.life-balance.exchange+xml";
  } else if (extension === "les") {
    return "application/vnd.hhe.lesson-player";
  } else if (extension === "lha") {
    return "application/x-lzh-compressed";
  } else if (extension === "link66") {
    return "application/vnd.route66.link66+xml";
  } else if (extension === "list") {
    return "text/plain";
  } else if (extension === "list3820") {
    return "application/vnd.ibm.modcap";
  } else if (extension === "listafp") {
    return "application/vnd.ibm.modcap";
  } else if (extension === "lnk") {
    return "application/x-ms-shortcut";
  } else if (extension === "log") {
    return "text/plain";
  } else if (extension === "lostxml") {
    return "application/lost+xml";
  } else if (extension === "lrf") {
    return "application/octet-stream";
  } else if (extension === "lrm") {
    return "application/vnd.ms-lrm";
  } else if (extension === "ltf") {
    return "application/vnd.frogans.ltf";
  } else if (extension === "lvp") {
    return "audio/vnd.lucent.voice";
  } else if (extension === "lwp") {
    return "application/vnd.lotus-wordpro";
  } else if (extension === "lzh") {
    return "application/x-lzh-compressed";
  } else if (extension === "m13") {
    return "application/x-msmediaview";
  } else if (extension === "m14") {
    return "application/x-msmediaview";
  } else if (extension === "m1v") {
    return "video/mpeg";
  } else if (extension === "m21") {
    return "application/mp21";
  } else if (extension === "m2a") {
    return "audio/mpeg";
  } else if (extension === "m2t") {
    return "video/mp2t";
  } else if (extension === "m2ts") {
    return "video/mp2t";
  } else if (extension === "m2v") {
    return "video/mpeg";
  } else if (extension === "m3a") {
    return "audio/mpeg";
  } else if (extension === "m3u") {
    return "audio/x-mpegurl";
  } else if (extension === "m3u8") {
    return "application/vnd.apple.mpegurl";
  } else if (extension === "m4a") {
    return "audio/mp4";
  } else if (extension === "m4u") {
    return "video/vnd.mpegurl";
  } else if (extension === "m4v") {
    return "video/x-m4v";
  } else if (extension === "ma") {
    return "application/mathematica";
  } else if (extension === "mads") {
    return "application/mads+xml";
  } else if (extension === "mag") {
    return "application/vnd.ecowin.chart";
  } else if (extension === "maker") {
    return "application/vnd.framemaker";
  } else if (extension === "man") {
    return "text/troff";
  } else if (extension === "mar") {
    return "application/octet-stream";
  } else if (extension === "mathml") {
    return "application/mathml+xml";
  } else if (extension === "mb") {
    return "application/mathematica";
  } else if (extension === "mbk") {
    return "application/vnd.mobius.mbk";
  } else if (extension === "mbox") {
    return "application/mbox";
  } else if (extension === "mc1") {
    return "application/vnd.medcalcdata";
  } else if (extension === "mcd") {
    return "application/vnd.mcd";
  } else if (extension === "mcurl") {
    return "text/vnd.curl.mcurl";
  } else if (extension === "mdb") {
    return "application/x-msaccess";
  } else if (extension === "mdi") {
    return "image/vnd.ms-modi";
  } else if (extension === "me") {
    return "text/troff";
  } else if (extension === "mesh") {
    return "model/mesh";
  } else if (extension === "meta4") {
    return "application/metalink4+xml";
  } else if (extension === "metalink") {
    return "application/metalink+xml";
  } else if (extension === "mets") {
    return "application/mets+xml";
  } else if (extension === "mfm") {
    return "application/vnd.mfmp";
  } else if (extension === "mft") {
    return "application/rpki-manifest";
  } else if (extension === "mgp") {
    return "application/vnd.osgeo.mapguide.package";
  } else if (extension === "mgz") {
    return "application/vnd.proteus.magazine";
  } else if (extension === "mid") {
    return "audio/midi";
  } else if (extension === "midi") {
    return "audio/midi";
  } else if (extension === "mie") {
    return "application/x-mie";
  } else if (extension === "mif") {
    return "application/vnd.mif";
  } else if (extension === "mime") {
    return "message/rfc822";
  } else if (extension === "mj2") {
    return "video/mj2";
  } else if (extension === "mjp2") {
    return "video/mj2";
  } else if (extension === "mjs") {
    return "text/javascript";
  } else if (extension === "mk3d") {
    return "video/x-matroska";
  } else if (extension === "mka") {
    return "audio/x-matroska";
  } else if (extension === "mks") {
    return "video/x-matroska";
  } else if (extension === "mkv") {
    return "video/x-matroska";
  } else if (extension === "mlp") {
    return "application/vnd.dolby.mlp";
  } else if (extension === "mmd") {
    return "application/vnd.chipnuts.karaoke-mmd";
  } else if (extension === "mmf") {
    return "application/vnd.smaf";
  } else if (extension === "mmr") {
    return "image/vnd.fujixerox.edmics-mmr";
  } else if (extension === "mng") {
    return "video/x-mng";
  } else if (extension === "mny") {
    return "application/x-msmoney";
  } else if (extension === "mobi") {
    return "application/x-mobipocket-ebook";
  } else if (extension === "mods") {
    return "application/mods+xml";
  } else if (extension === "mov") {
    return "video/quicktime";
  } else if (extension === "movie") {
    return "video/x-sgi-movie";
  } else if (extension === "mp2") {
    return "audio/mpeg";
  } else if (extension === "mp21") {
    return "application/mp21";
  } else if (extension === "mp2a") {
    return "audio/mpeg";
  } else if (extension === "mp3") {
    return "audio/mpeg";
  } else if (extension === "mp4") {
    return "video/mp4";
  } else if (extension === "mp4a") {
    return "audio/mp4";
  } else if (extension === "mp4s") {
    return "application/mp4";
  } else if (extension === "mp4v") {
    return "video/mp4";
  } else if (extension === "mpc") {
    return "application/vnd.mophun.certificate";
  } else if (extension === "mpe") {
    return "video/mpeg";
  } else if (extension === "mpeg") {
    return "video/mpeg";
  } else if (extension === "mpg") {
    return "video/mpeg";
  } else if (extension === "mpg4") {
    return "video/mp4";
  } else if (extension === "mpga") {
    return "audio/mpeg";
  } else if (extension === "mpkg") {
    return "application/vnd.apple.installer+xml";
  } else if (extension === "mpm") {
    return "application/vnd.blueice.multipass";
  } else if (extension === "mpn") {
    return "application/vnd.mophun.application";
  } else if (extension === "mpp") {
    return "application/vnd.ms-project";
  } else if (extension === "mpt") {
    return "application/vnd.ms-project";
  } else if (extension === "mpy") {
    return "application/vnd.ibm.minipay";
  } else if (extension === "mqy") {
    return "application/vnd.mobius.mqy";
  } else if (extension === "mrc") {
    return "application/marc";
  } else if (extension === "mrcx") {
    return "application/marcxml+xml";
  } else if (extension === "ms") {
    return "text/troff";
  } else if (extension === "mscml") {
    return "application/mediaservercontrol+xml";
  } else if (extension === "mseed") {
    return "application/vnd.fdsn.mseed";
  } else if (extension === "mseq") {
    return "application/vnd.mseq";
  } else if (extension === "msf") {
    return "application/vnd.epson.msf";
  } else if (extension === "msh") {
    return "model/mesh";
  } else if (extension === "msi") {
    return "application/x-msdownload";
  } else if (extension === "msl") {
    return "application/vnd.mobius.msl";
  } else if (extension === "msty") {
    return "application/vnd.muvee.style";
  } else if (extension === "mts") {
    return "video/mp2t";
  } else if (extension === "mus") {
    return "application/vnd.musician";
  } else if (extension === "musicxml") {
    return "application/vnd.recordare.musicxml+xml";
  } else if (extension === "mvb") {
    return "application/x-msmediaview";
  } else if (extension === "mwf") {
    return "application/vnd.mfer";
  } else if (extension === "mxf") {
    return "application/mxf";
  } else if (extension === "mxl") {
    return "application/vnd.recordare.musicxml";
  } else if (extension === "mxml") {
    return "application/xv+xml";
  } else if (extension === "mxs") {
    return "application/vnd.triscape.mxs";
  } else if (extension === "mxu") {
    return "video/vnd.mpegurl";
  } else if (extension === "n-gage") {
    return "application/vnd.nokia.n-gage.symbian.install";
  } else if (extension === "n3") {
    return "text/n3";
  } else if (extension === "nb") {
    return "application/mathematica";
  } else if (extension === "nbp") {
    return "application/vnd.wolfram.player";
  } else if (extension === "nc") {
    return "application/x-netcdf";
  } else if (extension === "ncx") {
    return "application/x-dtbncx+xml";
  } else if (extension === "nfo") {
    return "text/x-nfo";
  } else if (extension === "ngdat") {
    return "application/vnd.nokia.n-gage.data";
  } else if (extension === "nitf") {
    return "application/vnd.nitf";
  } else if (extension === "nlu") {
    return "application/vnd.neurolanguage.nlu";
  } else if (extension === "nml") {
    return "application/vnd.enliven";
  } else if (extension === "nnd") {
    return "application/vnd.noblenet-directory";
  } else if (extension === "nns") {
    return "application/vnd.noblenet-sealer";
  } else if (extension === "nnw") {
    return "application/vnd.noblenet-web";
  } else if (extension === "npx") {
    return "image/vnd.net-fpx";
  } else if (extension === "nsc") {
    return "application/x-conference";
  } else if (extension === "nsf") {
    return "application/vnd.lotus-notes";
  } else if (extension === "ntf") {
    return "application/vnd.nitf";
  } else if (extension === "nzb") {
    return "application/x-nzb";
  } else if (extension === "oa2") {
    return "application/vnd.fujitsu.oasys2";
  } else if (extension === "oa3") {
    return "application/vnd.fujitsu.oasys3";
  } else if (extension === "oas") {
    return "application/vnd.fujitsu.oasys";
  } else if (extension === "obd") {
    return "application/x-msbinder";
  } else if (extension === "obj") {
    return "application/x-tgif";
  } else if (extension === "oda") {
    return "application/oda";
  } else if (extension === "odb") {
    return "application/vnd.oasis.opendocument.database";
  } else if (extension === "odc") {
    return "application/vnd.oasis.opendocument.chart";
  } else if (extension === "odf") {
    return "application/vnd.oasis.opendocument.formula";
  } else if (extension === "odft") {
    return "application/vnd.oasis.opendocument.formula-template";
  } else if (extension === "odg") {
    return "application/vnd.oasis.opendocument.graphics";
  } else if (extension === "odi") {
    return "application/vnd.oasis.opendocument.image";
  } else if (extension === "odm") {
    return "application/vnd.oasis.opendocument.text-master";
  } else if (extension === "odp") {
    return "application/vnd.oasis.opendocument.presentation";
  } else if (extension === "ods") {
    return "application/vnd.oasis.opendocument.spreadsheet";
  } else if (extension === "odt") {
    return "application/vnd.oasis.opendocument.text";
  } else if (extension === "oga") {
    return "audio/ogg";
  } else if (extension === "ogg") {
    return "audio/ogg";
  } else if (extension === "ogv") {
    return "video/ogg";
  } else if (extension === "ogx") {
    return "application/ogg";
  } else if (extension === "omdoc") {
    return "application/omdoc+xml";
  } else if (extension === "onepkg") {
    return "application/onenote";
  } else if (extension === "onetmp") {
    return "application/onenote";
  } else if (extension === "onetoc") {
    return "application/onenote";
  } else if (extension === "onetoc2") {
    return "application/onenote";
  } else if (extension === "opf") {
    return "application/oebps-package+xml";
  } else if (extension === "opml") {
    return "text/x-opml";
  } else if (extension === "oprc") {
    return "application/vnd.palm";
  } else if (extension === "opus") {
    return "audio/ogg";
  } else if (extension === "org") {
    return "application/vnd.lotus-organizer";
  } else if (extension === "osf") {
    return "application/vnd.yamaha.openscoreformat";
  } else if (extension === "osfpvg") {
    return "application/vnd.yamaha.openscoreformat.osfpvg+xml";
  } else if (extension === "otc") {
    return "application/vnd.oasis.opendocument.chart-template";
  } else if (extension === "otf") {
    return "font/otf";
  } else if (extension === "otg") {
    return "application/vnd.oasis.opendocument.graphics-template";
  } else if (extension === "oth") {
    return "application/vnd.oasis.opendocument.text-web";
  } else if (extension === "oti") {
    return "application/vnd.oasis.opendocument.image-template";
  } else if (extension === "otp") {
    return "application/vnd.oasis.opendocument.presentation-template";
  } else if (extension === "ots") {
    return "application/vnd.oasis.opendocument.spreadsheet-template";
  } else if (extension === "ott") {
    return "application/vnd.oasis.opendocument.text-template";
  } else if (extension === "oxps") {
    return "application/oxps";
  } else if (extension === "oxt") {
    return "application/vnd.openofficeorg.extension";
  } else if (extension === "p") {
    return "text/x-pascal";
  } else if (extension === "p10") {
    return "application/pkcs10";
  } else if (extension === "p12") {
    return "application/x-pkcs12";
  } else if (extension === "p7b") {
    return "application/x-pkcs7-certificates";
  } else if (extension === "p7c") {
    return "application/pkcs7-mime";
  } else if (extension === "p7m") {
    return "application/pkcs7-mime";
  } else if (extension === "p7r") {
    return "application/x-pkcs7-certreqresp";
  } else if (extension === "p7s") {
    return "application/pkcs7-signature";
  } else if (extension === "p8") {
    return "application/pkcs8";
  } else if (extension === "pas") {
    return "text/x-pascal";
  } else if (extension === "paw") {
    return "application/vnd.pawaafile";
  } else if (extension === "pbd") {
    return "application/vnd.powerbuilder6";
  } else if (extension === "pbm") {
    return "image/x-portable-bitmap";
  } else if (extension === "pcap") {
    return "application/vnd.tcpdump.pcap";
  } else if (extension === "pcf") {
    return "application/x-font-pcf";
  } else if (extension === "pcl") {
    return "application/vnd.hp-pcl";
  } else if (extension === "pclxl") {
    return "application/vnd.hp-pclxl";
  } else if (extension === "pct") {
    return "image/x-pict";
  } else if (extension === "pcurl") {
    return "application/vnd.curl.pcurl";
  } else if (extension === "pcx") {
    return "image/x-pcx";
  } else if (extension === "pdb") {
    return "application/vnd.palm";
  } else if (extension === "pdf") {
    return "application/pdf";
  } else if (extension === "pfa") {
    return "application/x-font-type1";
  } else if (extension === "pfb") {
    return "application/x-font-type1";
  } else if (extension === "pfm") {
    return "application/x-font-type1";
  } else if (extension === "pfr") {
    return "application/font-tdpfr";
  } else if (extension === "pfx") {
    return "application/x-pkcs12";
  } else if (extension === "pgm") {
    return "image/x-portable-graymap";
  } else if (extension === "pgn") {
    return "application/x-chess-pgn";
  } else if (extension === "pgp") {
    return "application/pgp-encrypted";
  } else if (extension === "pic") {
    return "image/x-pict";
  } else if (extension === "pkg") {
    return "application/octet-stream";
  } else if (extension === "pki") {
    return "application/pkixcmp";
  } else if (extension === "pkipath") {
    return "application/pkix-pkipath";
  } else if (extension === "plb") {
    return "application/vnd.3gpp.pic-bw-large";
  } else if (extension === "plc") {
    return "application/vnd.mobius.plc";
  } else if (extension === "plf") {
    return "application/vnd.pocketlearn";
  } else if (extension === "pls") {
    return "application/pls+xml";
  } else if (extension === "pml") {
    return "application/vnd.ctc-posml";
  } else if (extension === "png") {
    return "image/png";
  } else if (extension === "pnm") {
    return "image/x-portable-anymap";
  } else if (extension === "portpkg") {
    return "application/vnd.macports.portpkg";
  } else if (extension === "pot") {
    return "application/vnd.ms-powerpoint";
  } else if (extension === "potm") {
    return "application/vnd.ms-powerpoint.template.macroenabled.12";
  } else if (extension === "potx") {
    return "application/vnd.openxmlformats-officedocument.presentationml.template";
  } else if (extension === "ppam") {
    return "application/vnd.ms-powerpoint.addin.macroenabled.12";
  } else if (extension === "ppd") {
    return "application/vnd.cups-ppd";
  } else if (extension === "ppm") {
    return "image/x-portable-pixmap";
  } else if (extension === "pps") {
    return "application/vnd.ms-powerpoint";
  } else if (extension === "ppsm") {
    return "application/vnd.ms-powerpoint.slideshow.macroenabled.12";
  } else if (extension === "ppsx") {
    return "application/vnd.openxmlformats-officedocument.presentationml.slideshow";
  } else if (extension === "ppt") {
    return "application/vnd.ms-powerpoint";
  } else if (extension === "pptm") {
    return "application/vnd.ms-powerpoint.presentation.macroenabled.12";
  } else if (extension === "pptx") {
    return "application/vnd.openxmlformats-officedocument.presentationml.presentation";
  } else if (extension === "pqa") {
    return "application/vnd.palm";
  } else if (extension === "prc") {
    return "application/x-mobipocket-ebook";
  } else if (extension === "pre") {
    return "application/vnd.lotus-freelance";
  } else if (extension === "prf") {
    return "application/pics-rules";
  } else if (extension === "ps") {
    return "application/postscript";
  } else if (extension === "psb") {
    return "application/vnd.3gpp.pic-bw-small";
  } else if (extension === "psd") {
    return "image/vnd.adobe.photoshop";
  } else if (extension === "psf") {
    return "application/x-font-linux-psf";
  } else if (extension === "pskcxml") {
    return "application/pskc+xml";
  } else if (extension === "ptid") {
    return "application/vnd.pvi.ptid1";
  } else if (extension === "pub") {
    return "application/x-mspublisher";
  } else if (extension === "pvb") {
    return "application/vnd.3gpp.pic-bw-var";
  } else if (extension === "pwn") {
    return "application/vnd.3m.post-it-notes";
  } else if (extension === "pya") {
    return "audio/vnd.ms-playready.media.pya";
  } else if (extension === "pyv") {
    return "video/vnd.ms-playready.media.pyv";
  } else if (extension === "qam") {
    return "application/vnd.epson.quickanime";
  } else if (extension === "qbo") {
    return "application/vnd.intu.qbo";
  } else if (extension === "qfx") {
    return "application/vnd.intu.qfx";
  } else if (extension === "qps") {
    return "application/vnd.publishare-delta-tree";
  } else if (extension === "qt") {
    return "video/quicktime";
  } else if (extension === "qwd") {
    return "application/vnd.quark.quarkxpress";
  } else if (extension === "qwt") {
    return "application/vnd.quark.quarkxpress";
  } else if (extension === "qxb") {
    return "application/vnd.quark.quarkxpress";
  } else if (extension === "qxd") {
    return "application/vnd.quark.quarkxpress";
  } else if (extension === "qxl") {
    return "application/vnd.quark.quarkxpress";
  } else if (extension === "qxt") {
    return "application/vnd.quark.quarkxpress";
  } else if (extension === "ra") {
    return "audio/x-pn-realaudio";
  } else if (extension === "ram") {
    return "audio/x-pn-realaudio";
  } else if (extension === "rar") {
    return "application/x-rar-compressed";
  } else if (extension === "ras") {
    return "image/x-cmu-raster";
  } else if (extension === "rcprofile") {
    return "application/vnd.ipunplugged.rcprofile";
  } else if (extension === "rdf") {
    return "application/rdf+xml";
  } else if (extension === "rdz") {
    return "application/vnd.data-vision.rdz";
  } else if (extension === "rep") {
    return "application/vnd.businessobjects";
  } else if (extension === "res") {
    return "application/x-dtbresource+xml";
  } else if (extension === "rgb") {
    return "image/x-rgb";
  } else if (extension === "rif") {
    return "application/reginfo+xml";
  } else if (extension === "rip") {
    return "audio/vnd.rip";
  } else if (extension === "ris") {
    return "application/x-research-info-systems";
  } else if (extension === "rl") {
    return "application/resource-lists+xml";
  } else if (extension === "rlc") {
    return "image/vnd.fujixerox.edmics-rlc";
  } else if (extension === "rld") {
    return "application/resource-lists-diff+xml";
  } else if (extension === "rm") {
    return "application/vnd.rn-realmedia";
  } else if (extension === "rmi") {
    return "audio/midi";
  } else if (extension === "rmp") {
    return "audio/x-pn-realaudio-plugin";
  } else if (extension === "rms") {
    return "application/vnd.jcp.javame.midlet-rms";
  } else if (extension === "rmvb") {
    return "application/vnd.rn-realmedia-vbr";
  } else if (extension === "rnc") {
    return "application/relax-ng-compact-syntax";
  } else if (extension === "roa") {
    return "application/rpki-roa";
  } else if (extension === "roff") {
    return "text/troff";
  } else if (extension === "rp9") {
    return "application/vnd.cloanto.rp9";
  } else if (extension === "rpss") {
    return "application/vnd.nokia.radio-presets";
  } else if (extension === "rpst") {
    return "application/vnd.nokia.radio-preset";
  } else if (extension === "rq") {
    return "application/sparql-query";
  } else if (extension === "rs") {
    return "application/rls-services+xml";
  } else if (extension === "rsd") {
    return "application/rsd+xml";
  } else if (extension === "rss") {
    return "application/rss+xml";
  } else if (extension === "rtf") {
    return "application/rtf";
  } else if (extension === "rtx") {
    return "text/richtext";
  } else if (extension === "s") {
    return "text/x-asm";
  } else if (extension === "s3m") {
    return "audio/s3m";
  } else if (extension === "saf") {
    return "application/vnd.yamaha.smaf-audio";
  } else if (extension === "sbml") {
    return "application/sbml+xml";
  } else if (extension === "sc") {
    return "application/vnd.ibm.secure-container";
  } else if (extension === "scd") {
    return "application/x-msschedule";
  } else if (extension === "scm") {
    return "application/vnd.lotus-screencam";
  } else if (extension === "scq") {
    return "application/scvp-cv-request";
  } else if (extension === "scs") {
    return "application/scvp-cv-response";
  } else if (extension === "scurl") {
    return "text/vnd.curl.scurl";
  } else if (extension === "sda") {
    return "application/vnd.stardivision.draw";
  } else if (extension === "sdc") {
    return "application/vnd.stardivision.calc";
  } else if (extension === "sdd") {
    return "application/vnd.stardivision.impress";
  } else if (extension === "sdkd") {
    return "application/vnd.solent.sdkm+xml";
  } else if (extension === "sdkm") {
    return "application/vnd.solent.sdkm+xml";
  } else if (extension === "sdp") {
    return "application/sdp";
  } else if (extension === "sdw") {
    return "application/vnd.stardivision.writer";
  } else if (extension === "see") {
    return "application/vnd.seemail";
  } else if (extension === "seed") {
    return "application/vnd.fdsn.seed";
  } else if (extension === "sema") {
    return "application/vnd.sema";
  } else if (extension === "semd") {
    return "application/vnd.semd";
  } else if (extension === "semf") {
    return "application/vnd.semf";
  } else if (extension === "ser") {
    return "application/java-serialized-object";
  } else if (extension === "setpay") {
    return "application/set-payment-initiation";
  } else if (extension === "setreg") {
    return "application/set-registration-initiation";
  } else if (extension === "sfd-hdstx") {
    return "application/vnd.hydrostatix.sof-data";
  } else if (extension === "sfs") {
    return "application/vnd.spotfire.sfs";
  } else if (extension === "sfv") {
    return "text/x-sfv";
  } else if (extension === "sgi") {
    return "image/sgi";
  } else if (extension === "sgl") {
    return "application/vnd.stardivision.writer-global";
  } else if (extension === "sgm") {
    return "text/sgml";
  } else if (extension === "sgml") {
    return "text/sgml";
  } else if (extension === "sh") {
    return "application/x-sh";
  } else if (extension === "shar") {
    return "application/x-shar";
  } else if (extension === "shf") {
    return "application/shf+xml";
  } else if (extension === "sid") {
    return "image/x-mrsid-image";
  } else if (extension === "sig") {
    return "application/pgp-signature";
  } else if (extension === "sil") {
    return "audio/silk";
  } else if (extension === "silo") {
    return "model/mesh";
  } else if (extension === "sis") {
    return "application/vnd.symbian.install";
  } else if (extension === "sisx") {
    return "application/vnd.symbian.install";
  } else if (extension === "sit") {
    return "application/x-stuffit";
  } else if (extension === "sitx") {
    return "application/x-stuffitx";
  } else if (extension === "skd") {
    return "application/vnd.koan";
  } else if (extension === "skm") {
    return "application/vnd.koan";
  } else if (extension === "skp") {
    return "application/vnd.koan";
  } else if (extension === "skt") {
    return "application/vnd.koan";
  } else if (extension === "sldm") {
    return "application/vnd.ms-powerpoint.slide.macroenabled.12";
  } else if (extension === "sldx") {
    return "application/vnd.openxmlformats-officedocument.presentationml.slide";
  } else if (extension === "slt") {
    return "application/vnd.epson.salt";
  } else if (extension === "sm") {
    return "application/vnd.stepmania.stepchart";
  } else if (extension === "smf") {
    return "application/vnd.stardivision.math";
  } else if (extension === "smi") {
    return "application/smil+xml";
  } else if (extension === "smil") {
    return "application/smil+xml";
  } else if (extension === "smv") {
    return "video/x-smv";
  } else if (extension === "smzip") {
    return "application/vnd.stepmania.package";
  } else if (extension === "snd") {
    return "audio/basic";
  } else if (extension === "snf") {
    return "application/x-font-snf";
  } else if (extension === "so") {
    return "application/octet-stream";
  } else if (extension === "spc") {
    return "application/x-pkcs7-certificates";
  } else if (extension === "spf") {
    return "application/vnd.yamaha.smaf-phrase";
  } else if (extension === "spl") {
    return "application/x-futuresplash";
  } else if (extension === "spot") {
    return "text/vnd.in3d.spot";
  } else if (extension === "spp") {
    return "application/scvp-vp-response";
  } else if (extension === "spq") {
    return "application/scvp-vp-request";
  } else if (extension === "spx") {
    return "audio/ogg";
  } else if (extension === "sql") {
    return "application/x-sql";
  } else if (extension === "src") {
    return "application/x-wais-source";
  } else if (extension === "srt") {
    return "application/x-subrip";
  } else if (extension === "sru") {
    return "application/sru+xml";
  } else if (extension === "srx") {
    return "application/sparql-results+xml";
  } else if (extension === "ssdl") {
    return "application/ssdl+xml";
  } else if (extension === "sse") {
    return "application/vnd.kodak-descriptor";
  } else if (extension === "ssf") {
    return "application/vnd.epson.ssf";
  } else if (extension === "ssml") {
    return "application/ssml+xml";
  } else if (extension === "st") {
    return "application/vnd.sailingtracker.track";
  } else if (extension === "stc") {
    return "application/vnd.sun.xml.calc.template";
  } else if (extension === "std") {
    return "application/vnd.sun.xml.draw.template";
  } else if (extension === "stf") {
    return "application/vnd.wt.stf";
  } else if (extension === "sti") {
    return "application/vnd.sun.xml.impress.template";
  } else if (extension === "stk") {
    return "application/hyperstudio";
  } else if (extension === "stl") {
    return "application/vnd.ms-pki.stl";
  } else if (extension === "str") {
    return "application/vnd.pg.format";
  } else if (extension === "stw") {
    return "application/vnd.sun.xml.writer.template";
  } else if (extension === "sub") {
    return "image/vnd.dvb.subtitle";
  } else if (extension === "sus") {
    return "application/vnd.sus-calendar";
  } else if (extension === "susp") {
    return "application/vnd.sus-calendar";
  } else if (extension === "sv4cpio") {
    return "application/x-sv4cpio";
  } else if (extension === "sv4crc") {
    return "application/x-sv4crc";
  } else if (extension === "svc") {
    return "application/vnd.dvb.service";
  } else if (extension === "svd") {
    return "application/vnd.svd";
  } else if (extension === "svg") {
    return "image/svg+xml";
  } else if (extension === "svgz") {
    return "image/svg+xml";
  } else if (extension === "swa") {
    return "application/x-director";
  } else if (extension === "swf") {
    return "application/x-shockwave-flash";
  } else if (extension === "swi") {
    return "application/vnd.aristanetworks.swi";
  } else if (extension === "sxc") {
    return "application/vnd.sun.xml.calc";
  } else if (extension === "sxd") {
    return "application/vnd.sun.xml.draw";
  } else if (extension === "sxg") {
    return "application/vnd.sun.xml.writer.global";
  } else if (extension === "sxi") {
    return "application/vnd.sun.xml.impress";
  } else if (extension === "sxm") {
    return "application/vnd.sun.xml.math";
  } else if (extension === "sxw") {
    return "application/vnd.sun.xml.writer";
  } else if (extension === "t") {
    return "text/troff";
  } else if (extension === "t3") {
    return "application/x-t3vm-image";
  } else if (extension === "taglet") {
    return "application/vnd.mynfc";
  } else if (extension === "tao") {
    return "application/vnd.tao.intent-module-archive";
  } else if (extension === "tar") {
    return "application/x-tar";
  } else if (extension === "tcap") {
    return "application/vnd.3gpp2.tcap";
  } else if (extension === "tcl") {
    return "application/x-tcl";
  } else if (extension === "teacher") {
    return "application/vnd.smart.teacher";
  } else if (extension === "tei") {
    return "application/tei+xml";
  } else if (extension === "teicorpus") {
    return "application/tei+xml";
  } else if (extension === "tex") {
    return "application/x-tex";
  } else if (extension === "texi") {
    return "application/x-texinfo";
  } else if (extension === "texinfo") {
    return "application/x-texinfo";
  } else if (extension === "text") {
    return "text/plain";
  } else if (extension === "tfi") {
    return "application/thraud+xml";
  } else if (extension === "tfm") {
    return "application/x-tex-tfm";
  } else if (extension === "tga") {
    return "image/x-tga";
  } else if (extension === "thmx") {
    return "application/vnd.ms-officetheme";
  } else if (extension === "tif") {
    return "image/tiff";
  } else if (extension === "tiff") {
    return "image/tiff";
  } else if (extension === "tmo") {
    return "application/vnd.tmobile-livetv";
  } else if (extension === "torrent") {
    return "application/x-bittorrent";
  } else if (extension === "tpl") {
    return "application/vnd.groove-tool-template";
  } else if (extension === "tpt") {
    return "application/vnd.trid.tpt";
  } else if (extension === "tr") {
    return "text/troff";
  } else if (extension === "tra") {
    return "application/vnd.trueapp";
  } else if (extension === "trm") {
    return "application/x-msterminal";
  } else if (extension === "ts") {
    return "video/mp2t";
  } else if (extension === "tsd") {
    return "application/timestamped-data";
  } else if (extension === "tsv") {
    return "text/tab-separated-values";
  } else if (extension === "ttc") {
    return "font/collection";
  } else if (extension === "ttf") {
    return "font/ttf";
  } else if (extension === "ttl") {
    return "text/turtle";
  } else if (extension === "twd") {
    return "application/vnd.simtech-mindmapper";
  } else if (extension === "twds") {
    return "application/vnd.simtech-mindmapper";
  } else if (extension === "txd") {
    return "application/vnd.genomatix.tuxedo";
  } else if (extension === "txf") {
    return "application/vnd.mobius.txf";
  } else if (extension === "txt") {
    return "text/plain";
  } else if (extension === "u32") {
    return "application/x-authorware-bin";
  } else if (extension === "udeb") {
    return "application/x-debian-package";
  } else if (extension === "ufd") {
    return "application/vnd.ufdl";
  } else if (extension === "ufdl") {
    return "application/vnd.ufdl";
  } else if (extension === "ulx") {
    return "application/x-glulx";
  } else if (extension === "umj") {
    return "application/vnd.umajin";
  } else if (extension === "unityweb") {
    return "application/vnd.unity";
  } else if (extension === "uoml") {
    return "application/vnd.uoml+xml";
  } else if (extension === "uri") {
    return "text/uri-list";
  } else if (extension === "uris") {
    return "text/uri-list";
  } else if (extension === "urls") {
    return "text/uri-list";
  } else if (extension === "ustar") {
    return "application/x-ustar";
  } else if (extension === "utz") {
    return "application/vnd.uiq.theme";
  } else if (extension === "uu") {
    return "text/x-uuencode";
  } else if (extension === "uva") {
    return "audio/vnd.dece.audio";
  } else if (extension === "uvd") {
    return "application/vnd.dece.data";
  } else if (extension === "uvf") {
    return "application/vnd.dece.data";
  } else if (extension === "uvg") {
    return "image/vnd.dece.graphic";
  } else if (extension === "uvh") {
    return "video/vnd.dece.hd";
  } else if (extension === "uvi") {
    return "image/vnd.dece.graphic";
  } else if (extension === "uvm") {
    return "video/vnd.dece.mobile";
  } else if (extension === "uvp") {
    return "video/vnd.dece.pd";
  } else if (extension === "uvs") {
    return "video/vnd.dece.sd";
  } else if (extension === "uvt") {
    return "application/vnd.dece.ttml+xml";
  } else if (extension === "uvu") {
    return "video/vnd.uvvu.mp4";
  } else if (extension === "uvv") {
    return "video/vnd.dece.video";
  } else if (extension === "uvva") {
    return "audio/vnd.dece.audio";
  } else if (extension === "uvvd") {
    return "application/vnd.dece.data";
  } else if (extension === "uvvf") {
    return "application/vnd.dece.data";
  } else if (extension === "uvvg") {
    return "image/vnd.dece.graphic";
  } else if (extension === "uvvh") {
    return "video/vnd.dece.hd";
  } else if (extension === "uvvi") {
    return "image/vnd.dece.graphic";
  } else if (extension === "uvvm") {
    return "video/vnd.dece.mobile";
  } else if (extension === "uvvp") {
    return "video/vnd.dece.pd";
  } else if (extension === "uvvs") {
    return "video/vnd.dece.sd";
  } else if (extension === "uvvt") {
    return "application/vnd.dece.ttml+xml";
  } else if (extension === "uvvu") {
    return "video/vnd.uvvu.mp4";
  } else if (extension === "uvvv") {
    return "video/vnd.dece.video";
  } else if (extension === "uvvx") {
    return "application/vnd.dece.unspecified";
  } else if (extension === "uvvz") {
    return "application/vnd.dece.zip";
  } else if (extension === "uvx") {
    return "application/vnd.dece.unspecified";
  } else if (extension === "uvz") {
    return "application/vnd.dece.zip";
  } else if (extension === "vcard") {
    return "text/vcard";
  } else if (extension === "vcd") {
    return "application/x-cdlink";
  } else if (extension === "vcf") {
    return "text/x-vcard";
  } else if (extension === "vcg") {
    return "application/vnd.groove-vcard";
  } else if (extension === "vcs") {
    return "text/x-vcalendar";
  } else if (extension === "vcx") {
    return "application/vnd.vcx";
  } else if (extension === "vis") {
    return "application/vnd.visionary";
  } else if (extension === "viv") {
    return "video/vnd.vivo";
  } else if (extension === "vob") {
    return "video/x-ms-vob";
  } else if (extension === "vor") {
    return "application/vnd.stardivision.writer";
  } else if (extension === "vox") {
    return "application/x-authorware-bin";
  } else if (extension === "vrml") {
    return "model/vrml";
  } else if (extension === "vsd") {
    return "application/vnd.visio";
  } else if (extension === "vsf") {
    return "application/vnd.vsf";
  } else if (extension === "vss") {
    return "application/vnd.visio";
  } else if (extension === "vst") {
    return "application/vnd.visio";
  } else if (extension === "vsw") {
    return "application/vnd.visio";
  } else if (extension === "vtu") {
    return "model/vnd.vtu";
  } else if (extension === "vxml") {
    return "application/voicexml+xml";
  } else if (extension === "w3d") {
    return "application/x-director";
  } else if (extension === "wad") {
    return "application/x-doom";
  } else if (extension === "wav") {
    return "audio/x-wav";
  } else if (extension === "wasm") {
    return "application/wasm";
  } else if (extension === "wax") {
    return "audio/x-ms-wax";
  } else if (extension === "wbmp") {
    return "image/vnd.wap.wbmp";
  } else if (extension === "wbs") {
    return "application/vnd.criticaltools.wbs+xml";
  } else if (extension === "wbxml") {
    return "application/vnd.wap.wbxml";
  } else if (extension === "wcm") {
    return "application/vnd.ms-works";
  } else if (extension === "wdb") {
    return "application/vnd.ms-works";
  } else if (extension === "wdp") {
    return "image/vnd.ms-photo";
  } else if (extension === "weba") {
    return "audio/webm";
  } else if (extension === "webm") {
    return "video/webm";
  } else if (extension === "webp") {
    return "image/webp";
  } else if (extension === "wg") {
    return "application/vnd.pmi.widget";
  } else if (extension === "wgt") {
    return "application/widget";
  } else if (extension === "wks") {
    return "application/vnd.ms-works";
  } else if (extension === "wm") {
    return "video/x-ms-wm";
  } else if (extension === "wma") {
    return "audio/x-ms-wma";
  } else if (extension === "wmd") {
    return "application/x-ms-wmd";
  } else if (extension === "wmf") {
    return "application/x-msmetafile";
  } else if (extension === "wml") {
    return "text/vnd.wap.wml";
  } else if (extension === "wmlc") {
    return "application/vnd.wap.wmlc";
  } else if (extension === "wmls") {
    return "text/vnd.wap.wmlscript";
  } else if (extension === "wmlsc") {
    return "application/vnd.wap.wmlscriptc";
  } else if (extension === "wmv") {
    return "video/x-ms-wmv";
  } else if (extension === "wmx") {
    return "video/x-ms-wmx";
  } else if (extension === "wmz") {
    return "application/x-msmetafile";
  } else if (extension === "woff") {
    return "font/woff";
  } else if (extension === "woff2") {
    return "font/woff2";
  } else if (extension === "wpd") {
    return "application/vnd.wordperfect";
  } else if (extension === "wpl") {
    return "application/vnd.ms-wpl";
  } else if (extension === "wps") {
    return "application/vnd.ms-works";
  } else if (extension === "wqd") {
    return "application/vnd.wqd";
  } else if (extension === "wri") {
    return "application/x-mswrite";
  } else if (extension === "wrl") {
    return "model/vrml";
  } else if (extension === "wsdl") {
    return "application/wsdl+xml";
  } else if (extension === "wspolicy") {
    return "application/wspolicy+xml";
  } else if (extension === "wtb") {
    return "application/vnd.webturbo";
  } else if (extension === "wvx") {
    return "video/x-ms-wvx";
  } else if (extension === "x32") {
    return "application/x-authorware-bin";
  } else if (extension === "x3d") {
    return "model/x3d+xml";
  } else if (extension === "x3db") {
    return "model/x3d+binary";
  } else if (extension === "x3dbz") {
    return "model/x3d+binary";
  } else if (extension === "x3dv") {
    return "model/x3d+vrml";
  } else if (extension === "x3dvz") {
    return "model/x3d+vrml";
  } else if (extension === "x3dz") {
    return "model/x3d+xml";
  } else if (extension === "xaml") {
    return "application/xaml+xml";
  } else if (extension === "xap") {
    return "application/x-silverlight-app";
  } else if (extension === "xar") {
    return "application/vnd.xara";
  } else if (extension === "xbap") {
    return "application/x-ms-xbap";
  } else if (extension === "xbd") {
    return "application/vnd.fujixerox.docuworks.binder";
  } else if (extension === "xbm") {
    return "image/x-xbitmap";
  } else if (extension === "xdf") {
    return "application/xcap-diff+xml";
  } else if (extension === "xdm") {
    return "application/vnd.syncml.dm+xml";
  } else if (extension === "xdp") {
    return "application/vnd.adobe.xdp+xml";
  } else if (extension === "xdssc") {
    return "application/dssc+xml";
  } else if (extension === "xdw") {
    return "application/vnd.fujixerox.docuworks";
  } else if (extension === "xenc") {
    return "application/xenc+xml";
  } else if (extension === "xer") {
    return "application/patch-ops-error+xml";
  } else if (extension === "xfdf") {
    return "application/vnd.adobe.xfdf";
  } else if (extension === "xfdl") {
    return "application/vnd.xfdl";
  } else if (extension === "xht") {
    return "application/xhtml+xml";
  } else if (extension === "xhtml") {
    return "application/xhtml+xml";
  } else if (extension === "xhvml") {
    return "application/xv+xml";
  } else if (extension === "xif") {
    return "image/vnd.xiff";
  } else if (extension === "xla") {
    return "application/vnd.ms-excel";
  } else if (extension === "xlam") {
    return "application/vnd.ms-excel.addin.macroenabled.12";
  } else if (extension === "xlc") {
    return "application/vnd.ms-excel";
  } else if (extension === "xlf") {
    return "application/x-xliff+xml";
  } else if (extension === "xlm") {
    return "application/vnd.ms-excel";
  } else if (extension === "xls") {
    return "application/vnd.ms-excel";
  } else if (extension === "xlsb") {
    return "application/vnd.ms-excel.sheet.binary.macroenabled.12";
  } else if (extension === "xlsm") {
    return "application/vnd.ms-excel.sheet.macroenabled.12";
  } else if (extension === "xlsx") {
    return "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
  } else if (extension === "xlt") {
    return "application/vnd.ms-excel";
  } else if (extension === "xltm") {
    return "application/vnd.ms-excel.template.macroenabled.12";
  } else if (extension === "xltx") {
    return "application/vnd.openxmlformats-officedocument.spreadsheetml.template";
  } else if (extension === "xlw") {
    return "application/vnd.ms-excel";
  } else if (extension === "xm") {
    return "audio/xm";
  } else if (extension === "xml") {
    return "application/xml";
  } else if (extension === "xo") {
    return "application/vnd.olpc-sugar";
  } else if (extension === "xop") {
    return "application/xop+xml";
  } else if (extension === "xpi") {
    return "application/x-xpinstall";
  } else if (extension === "xpl") {
    return "application/xproc+xml";
  } else if (extension === "xpm") {
    return "image/x-xpixmap";
  } else if (extension === "xpr") {
    return "application/vnd.is-xpr";
  } else if (extension === "xps") {
    return "application/vnd.ms-xpsdocument";
  } else if (extension === "xpw") {
    return "application/vnd.intercon.formnet";
  } else if (extension === "xpx") {
    return "application/vnd.intercon.formnet";
  } else if (extension === "xsl") {
    return "application/xml";
  } else if (extension === "xslt") {
    return "application/xslt+xml";
  } else if (extension === "xsm") {
    return "application/vnd.syncml+xml";
  } else if (extension === "xspf") {
    return "application/xspf+xml";
  } else if (extension === "xul") {
    return "application/vnd.mozilla.xul+xml";
  } else if (extension === "xvm") {
    return "application/xv+xml";
  } else if (extension === "xvml") {
    return "application/xv+xml";
  } else if (extension === "xwd") {
    return "image/x-xwindowdump";
  } else if (extension === "xyz") {
    return "chemical/x-xyz";
  } else if (extension === "xz") {
    return "application/x-xz";
  } else if (extension === "yang") {
    return "application/yang";
  } else if (extension === "yin") {
    return "application/yin+xml";
  } else if (extension === "z1") {
    return "application/x-zmachine";
  } else if (extension === "z2") {
    return "application/x-zmachine";
  } else if (extension === "z3") {
    return "application/x-zmachine";
  } else if (extension === "z4") {
    return "application/x-zmachine";
  } else if (extension === "z5") {
    return "application/x-zmachine";
  } else if (extension === "z6") {
    return "application/x-zmachine";
  } else if (extension === "z7") {
    return "application/x-zmachine";
  } else if (extension === "z8") {
    return "application/x-zmachine";
  } else if (extension === "zaz") {
    return "application/vnd.zzazz.deck+xml";
  } else if (extension === "zip") {
    return "application/zip";
  } else if (extension === "zir") {
    return "application/vnd.zul";
  } else if (extension === "zirz") {
    return "application/vnd.zul";
  } else if (extension === "zmm") {
    return "application/vnd.handheld-entertainment+xml";
  } else {
    return "application/octet-stream";
  }
}

export function mime_type_to_extensions(mime_type) {
  if (mime_type === "application/andrew-inset") {
    return toList(["ez"]);
  } else if (mime_type === "application/applixware") {
    return toList(["aw"]);
  } else if (mime_type === "application/atom+xml") {
    return toList(["atom"]);
  } else if (mime_type === "application/atomcat+xml") {
    return toList(["atomcat"]);
  } else if (mime_type === "application/atomsvc+xml") {
    return toList(["atomsvc"]);
  } else if (mime_type === "application/ccxml+xml") {
    return toList(["ccxml"]);
  } else if (mime_type === "application/cdmi-capability") {
    return toList(["cdmia"]);
  } else if (mime_type === "application/cdmi-container") {
    return toList(["cdmic"]);
  } else if (mime_type === "application/cdmi-domain") {
    return toList(["cdmid"]);
  } else if (mime_type === "application/cdmi-object") {
    return toList(["cdmio"]);
  } else if (mime_type === "application/cdmi-queue") {
    return toList(["cdmiq"]);
  } else if (mime_type === "application/cu-seeme") {
    return toList(["cu"]);
  } else if (mime_type === "application/davmount+xml") {
    return toList(["davmount"]);
  } else if (mime_type === "application/docbook+xml") {
    return toList(["dbk"]);
  } else if (mime_type === "application/dssc+der") {
    return toList(["dssc"]);
  } else if (mime_type === "application/dssc+xml") {
    return toList(["xdssc"]);
  } else if (mime_type === "application/ecmascript") {
    return toList(["ecma"]);
  } else if (mime_type === "application/emma+xml") {
    return toList(["emma"]);
  } else if (mime_type === "application/epub+zip") {
    return toList(["epub"]);
  } else if (mime_type === "application/exi") {
    return toList(["exi"]);
  } else if (mime_type === "application/font-tdpfr") {
    return toList(["pfr"]);
  } else if (mime_type === "application/gml+xml") {
    return toList(["gml"]);
  } else if (mime_type === "application/gpx+xml") {
    return toList(["gpx"]);
  } else if (mime_type === "application/gxf") {
    return toList(["gxf"]);
  } else if (mime_type === "application/hyperstudio") {
    return toList(["stk"]);
  } else if (mime_type === "application/inkml+xml") {
    return toList(["ink", "inkml"]);
  } else if (mime_type === "application/ipfix") {
    return toList(["ipfix"]);
  } else if (mime_type === "application/java-archive") {
    return toList(["jar"]);
  } else if (mime_type === "application/java-serialized-object") {
    return toList(["ser"]);
  } else if (mime_type === "application/java-vm") {
    return toList(["class"]);
  } else if (mime_type === "application/javascript") {
    return toList(["js", "mjs"]);
  } else if (mime_type === "application/json") {
    return toList(["json"]);
  } else if (mime_type === "application/jsonml+json") {
    return toList(["jsonml"]);
  } else if (mime_type === "application/lost+xml") {
    return toList(["lostxml"]);
  } else if (mime_type === "application/mac-binhex40") {
    return toList(["hqx"]);
  } else if (mime_type === "application/mac-compactpro") {
    return toList(["cpt"]);
  } else if (mime_type === "application/mads+xml") {
    return toList(["mads"]);
  } else if (mime_type === "application/marc") {
    return toList(["mrc"]);
  } else if (mime_type === "application/marcxml+xml") {
    return toList(["mrcx"]);
  } else if (mime_type === "application/mathematica") {
    return toList(["ma", "nb", "mb"]);
  } else if (mime_type === "application/mathml+xml") {
    return toList(["mathml"]);
  } else if (mime_type === "application/mbox") {
    return toList(["mbox"]);
  } else if (mime_type === "application/mediaservercontrol+xml") {
    return toList(["mscml"]);
  } else if (mime_type === "application/metalink+xml") {
    return toList(["metalink"]);
  } else if (mime_type === "application/metalink4+xml") {
    return toList(["meta4"]);
  } else if (mime_type === "application/mets+xml") {
    return toList(["mets"]);
  } else if (mime_type === "application/mods+xml") {
    return toList(["mods"]);
  } else if (mime_type === "application/mp21") {
    return toList(["m21", "mp21"]);
  } else if (mime_type === "application/mp4") {
    return toList(["mp4s"]);
  } else if (mime_type === "application/msword") {
    return toList(["doc", "dot"]);
  } else if (mime_type === "application/mxf") {
    return toList(["mxf"]);
  } else if (mime_type === "application/octet-stream") {
    return toList([
      "bin",
      "dms",
      "lrf",
      "mar",
      "so",
      "dist",
      "distz",
      "pkg",
      "bpk",
      "dump",
      "elc",
      "deploy",
    ]);
  } else if (mime_type === "application/oda") {
    return toList(["oda"]);
  } else if (mime_type === "application/oebps-package+xml") {
    return toList(["opf"]);
  } else if (mime_type === "application/ogg") {
    return toList(["ogx"]);
  } else if (mime_type === "application/omdoc+xml") {
    return toList(["omdoc"]);
  } else if (mime_type === "application/onenote") {
    return toList(["onetoc", "onetoc2", "onetmp", "onepkg"]);
  } else if (mime_type === "application/oxps") {
    return toList(["oxps"]);
  } else if (mime_type === "application/patch-ops-error+xml") {
    return toList(["xer"]);
  } else if (mime_type === "application/pdf") {
    return toList(["pdf"]);
  } else if (mime_type === "application/pgp-encrypted") {
    return toList(["pgp"]);
  } else if (mime_type === "application/pgp-signature") {
    return toList(["asc", "sig"]);
  } else if (mime_type === "application/pics-rules") {
    return toList(["prf"]);
  } else if (mime_type === "application/pkcs10") {
    return toList(["p10"]);
  } else if (mime_type === "application/pkcs7-mime") {
    return toList(["p7m", "p7c"]);
  } else if (mime_type === "application/pkcs7-signature") {
    return toList(["p7s"]);
  } else if (mime_type === "application/pkcs8") {
    return toList(["p8"]);
  } else if (mime_type === "application/pkix-attr-cert") {
    return toList(["ac"]);
  } else if (mime_type === "application/pkix-cert") {
    return toList(["cer"]);
  } else if (mime_type === "application/pkix-crl") {
    return toList(["crl"]);
  } else if (mime_type === "application/pkix-pkipath") {
    return toList(["pkipath"]);
  } else if (mime_type === "application/pkixcmp") {
    return toList(["pki"]);
  } else if (mime_type === "application/pls+xml") {
    return toList(["pls"]);
  } else if (mime_type === "application/postscript") {
    return toList(["ai", "eps", "ps"]);
  } else if (mime_type === "application/prs.cww") {
    return toList(["cww"]);
  } else if (mime_type === "application/pskc+xml") {
    return toList(["pskcxml"]);
  } else if (mime_type === "application/rdf+xml") {
    return toList(["rdf"]);
  } else if (mime_type === "application/reginfo+xml") {
    return toList(["rif"]);
  } else if (mime_type === "application/relax-ng-compact-syntax") {
    return toList(["rnc"]);
  } else if (mime_type === "application/resource-lists+xml") {
    return toList(["rl"]);
  } else if (mime_type === "application/resource-lists-diff+xml") {
    return toList(["rld"]);
  } else if (mime_type === "application/rls-services+xml") {
    return toList(["rs"]);
  } else if (mime_type === "application/rpki-ghostbusters") {
    return toList(["gbr"]);
  } else if (mime_type === "application/rpki-manifest") {
    return toList(["mft"]);
  } else if (mime_type === "application/rpki-roa") {
    return toList(["roa"]);
  } else if (mime_type === "application/rsd+xml") {
    return toList(["rsd"]);
  } else if (mime_type === "application/rss+xml") {
    return toList(["rss"]);
  } else if (mime_type === "application/rtf") {
    return toList(["rtf"]);
  } else if (mime_type === "application/sbml+xml") {
    return toList(["sbml"]);
  } else if (mime_type === "application/scvp-cv-request") {
    return toList(["scq"]);
  } else if (mime_type === "application/scvp-cv-response") {
    return toList(["scs"]);
  } else if (mime_type === "application/scvp-vp-request") {
    return toList(["spq"]);
  } else if (mime_type === "application/scvp-vp-response") {
    return toList(["spp"]);
  } else if (mime_type === "application/sdp") {
    return toList(["sdp"]);
  } else if (mime_type === "application/set-payment-initiation") {
    return toList(["setpay"]);
  } else if (mime_type === "application/set-registration-initiation") {
    return toList(["setreg"]);
  } else if (mime_type === "application/shf+xml") {
    return toList(["shf"]);
  } else if (mime_type === "application/smil+xml") {
    return toList(["smi", "smil"]);
  } else if (mime_type === "application/sparql-query") {
    return toList(["rq"]);
  } else if (mime_type === "application/sparql-results+xml") {
    return toList(["srx"]);
  } else if (mime_type === "application/srgs") {
    return toList(["gram"]);
  } else if (mime_type === "application/srgs+xml") {
    return toList(["grxml"]);
  } else if (mime_type === "application/sru+xml") {
    return toList(["sru"]);
  } else if (mime_type === "application/ssdl+xml") {
    return toList(["ssdl"]);
  } else if (mime_type === "application/ssml+xml") {
    return toList(["ssml"]);
  } else if (mime_type === "application/tei+xml") {
    return toList(["tei", "teicorpus"]);
  } else if (mime_type === "application/thraud+xml") {
    return toList(["tfi"]);
  } else if (mime_type === "application/timestamped-data") {
    return toList(["tsd"]);
  } else if (mime_type === "application/vnd.3gpp.pic-bw-large") {
    return toList(["plb"]);
  } else if (mime_type === "application/vnd.3gpp.pic-bw-small") {
    return toList(["psb"]);
  } else if (mime_type === "application/vnd.3gpp.pic-bw-var") {
    return toList(["pvb"]);
  } else if (mime_type === "application/vnd.3gpp2.tcap") {
    return toList(["tcap"]);
  } else if (mime_type === "application/vnd.3m.post-it-notes") {
    return toList(["pwn"]);
  } else if (mime_type === "application/vnd.accpac.simply.aso") {
    return toList(["aso"]);
  } else if (mime_type === "application/vnd.accpac.simply.imp") {
    return toList(["imp"]);
  } else if (mime_type === "application/vnd.acucobol") {
    return toList(["acu"]);
  } else if (mime_type === "application/vnd.acucorp") {
    return toList(["atc", "acutc"]);
  } else if (
    mime_type === "application/vnd.adobe.air-application-installer-package+zip"
  ) {
    return toList(["air"]);
  } else if (mime_type === "application/vnd.adobe.formscentral.fcdt") {
    return toList(["fcdt"]);
  } else if (mime_type === "application/vnd.adobe.fxp") {
    return toList(["fxp", "fxpl"]);
  } else if (mime_type === "application/vnd.adobe.xdp+xml") {
    return toList(["xdp"]);
  } else if (mime_type === "application/vnd.adobe.xfdf") {
    return toList(["xfdf"]);
  } else if (mime_type === "application/vnd.ahead.space") {
    return toList(["ahead"]);
  } else if (mime_type === "application/vnd.airzip.filesecure.azf") {
    return toList(["azf"]);
  } else if (mime_type === "application/vnd.airzip.filesecure.azs") {
    return toList(["azs"]);
  } else if (mime_type === "application/vnd.amazon.ebook") {
    return toList(["azw"]);
  } else if (mime_type === "application/vnd.americandynamics.acc") {
    return toList(["acc"]);
  } else if (mime_type === "application/vnd.amiga.ami") {
    return toList(["ami"]);
  } else if (mime_type === "application/vnd.android.package-archive") {
    return toList(["apk"]);
  } else if (
    mime_type === "application/vnd.anser-web-certificate-issue-initiation"
  ) {
    return toList(["cii"]);
  } else if (mime_type === "application/vnd.anser-web-funds-transfer-initiation") {
    return toList(["fti"]);
  } else if (mime_type === "application/vnd.antix.game-component") {
    return toList(["atx"]);
  } else if (mime_type === "application/vnd.apple.installer+xml") {
    return toList(["mpkg"]);
  } else if (mime_type === "application/vnd.apple.mpegurl") {
    return toList(["m3u8"]);
  } else if (mime_type === "application/vnd.aristanetworks.swi") {
    return toList(["swi"]);
  } else if (mime_type === "application/vnd.astraea-software.iota") {
    return toList(["iota"]);
  } else if (mime_type === "application/vnd.audiograph") {
    return toList(["aep"]);
  } else if (mime_type === "application/vnd.blueice.multipass") {
    return toList(["mpm"]);
  } else if (mime_type === "application/vnd.bmi") {
    return toList(["bmi"]);
  } else if (mime_type === "application/vnd.businessobjects") {
    return toList(["rep"]);
  } else if (mime_type === "application/vnd.chemdraw+xml") {
    return toList(["cdxml"]);
  } else if (mime_type === "application/vnd.chipnuts.karaoke-mmd") {
    return toList(["mmd"]);
  } else if (mime_type === "application/vnd.cinderella") {
    return toList(["cdy"]);
  } else if (mime_type === "application/vnd.claymore") {
    return toList(["cla"]);
  } else if (mime_type === "application/vnd.cloanto.rp9") {
    return toList(["rp9"]);
  } else if (mime_type === "application/vnd.clonk.c4group") {
    return toList(["c4g", "c4d", "c4f", "c4p", "c4u"]);
  } else if (mime_type === "application/vnd.cluetrust.cartomobile-config") {
    return toList(["c11amc"]);
  } else if (mime_type === "application/vnd.cluetrust.cartomobile-config-pkg") {
    return toList(["c11amz"]);
  } else if (mime_type === "application/vnd.commonspace") {
    return toList(["csp"]);
  } else if (mime_type === "application/vnd.contact.cmsg") {
    return toList(["cdbcmsg"]);
  } else if (mime_type === "application/vnd.cosmocaller") {
    return toList(["cmc"]);
  } else if (mime_type === "application/vnd.crick.clicker") {
    return toList(["clkx"]);
  } else if (mime_type === "application/vnd.crick.clicker.keyboard") {
    return toList(["clkk"]);
  } else if (mime_type === "application/vnd.crick.clicker.palette") {
    return toList(["clkp"]);
  } else if (mime_type === "application/vnd.crick.clicker.template") {
    return toList(["clkt"]);
  } else if (mime_type === "application/vnd.crick.clicker.wordbank") {
    return toList(["clkw"]);
  } else if (mime_type === "application/vnd.criticaltools.wbs+xml") {
    return toList(["wbs"]);
  } else if (mime_type === "application/vnd.ctc-posml") {
    return toList(["pml"]);
  } else if (mime_type === "application/vnd.cups-ppd") {
    return toList(["ppd"]);
  } else if (mime_type === "application/vnd.curl.car") {
    return toList(["car"]);
  } else if (mime_type === "application/vnd.curl.pcurl") {
    return toList(["pcurl"]);
  } else if (mime_type === "application/vnd.dart") {
    return toList(["dart"]);
  } else if (mime_type === "application/vnd.data-vision.rdz") {
    return toList(["rdz"]);
  } else if (mime_type === "application/vnd.dece.data") {
    return toList(["uvf", "uvvf", "uvd", "uvvd"]);
  } else if (mime_type === "application/vnd.dece.ttml+xml") {
    return toList(["uvt", "uvvt"]);
  } else if (mime_type === "application/vnd.dece.unspecified") {
    return toList(["uvx", "uvvx"]);
  } else if (mime_type === "application/vnd.dece.zip") {
    return toList(["uvz", "uvvz"]);
  } else if (mime_type === "application/vnd.denovo.fcselayout-link") {
    return toList(["fe_launch"]);
  } else if (mime_type === "application/vnd.dna") {
    return toList(["dna"]);
  } else if (mime_type === "application/vnd.dolby.mlp") {
    return toList(["mlp"]);
  } else if (mime_type === "application/vnd.dpgraph") {
    return toList(["dpg"]);
  } else if (mime_type === "application/vnd.dreamfactory") {
    return toList(["dfac"]);
  } else if (mime_type === "application/vnd.ds-keypoint") {
    return toList(["kpxx"]);
  } else if (mime_type === "application/vnd.dvb.ait") {
    return toList(["ait"]);
  } else if (mime_type === "application/vnd.dvb.service") {
    return toList(["svc"]);
  } else if (mime_type === "application/vnd.dynageo") {
    return toList(["geo"]);
  } else if (mime_type === "application/vnd.ecowin.chart") {
    return toList(["mag"]);
  } else if (mime_type === "application/vnd.enliven") {
    return toList(["nml"]);
  } else if (mime_type === "application/vnd.epson.esf") {
    return toList(["esf"]);
  } else if (mime_type === "application/vnd.epson.msf") {
    return toList(["msf"]);
  } else if (mime_type === "application/vnd.epson.quickanime") {
    return toList(["qam"]);
  } else if (mime_type === "application/vnd.epson.salt") {
    return toList(["slt"]);
  } else if (mime_type === "application/vnd.epson.ssf") {
    return toList(["ssf"]);
  } else if (mime_type === "application/vnd.eszigno3+xml") {
    return toList(["es3", "et3"]);
  } else if (mime_type === "application/vnd.ezpix-album") {
    return toList(["ez2"]);
  } else if (mime_type === "application/vnd.ezpix-package") {
    return toList(["ez3"]);
  } else if (mime_type === "application/vnd.fdf") {
    return toList(["fdf"]);
  } else if (mime_type === "application/vnd.fdsn.mseed") {
    return toList(["mseed"]);
  } else if (mime_type === "application/vnd.fdsn.seed") {
    return toList(["seed", "dataless"]);
  } else if (mime_type === "application/vnd.flographit") {
    return toList(["gph"]);
  } else if (mime_type === "application/vnd.fluxtime.clip") {
    return toList(["ftc"]);
  } else if (mime_type === "application/vnd.framemaker") {
    return toList(["fm", "frame", "maker", "book"]);
  } else if (mime_type === "application/vnd.frogans.fnc") {
    return toList(["fnc"]);
  } else if (mime_type === "application/vnd.frogans.ltf") {
    return toList(["ltf"]);
  } else if (mime_type === "application/vnd.fsc.weblaunch") {
    return toList(["fsc"]);
  } else if (mime_type === "application/vnd.fujitsu.oasys") {
    return toList(["oas"]);
  } else if (mime_type === "application/vnd.fujitsu.oasys2") {
    return toList(["oa2"]);
  } else if (mime_type === "application/vnd.fujitsu.oasys3") {
    return toList(["oa3"]);
  } else if (mime_type === "application/vnd.fujitsu.oasysgp") {
    return toList(["fg5"]);
  } else if (mime_type === "application/vnd.fujitsu.oasysprs") {
    return toList(["bh2"]);
  } else if (mime_type === "application/vnd.fujixerox.ddd") {
    return toList(["ddd"]);
  } else if (mime_type === "application/vnd.fujixerox.docuworks") {
    return toList(["xdw"]);
  } else if (mime_type === "application/vnd.fujixerox.docuworks.binder") {
    return toList(["xbd"]);
  } else if (mime_type === "application/vnd.fuzzysheet") {
    return toList(["fzs"]);
  } else if (mime_type === "application/vnd.genomatix.tuxedo") {
    return toList(["txd"]);
  } else if (mime_type === "application/vnd.geogebra.file") {
    return toList(["ggb"]);
  } else if (mime_type === "application/vnd.geogebra.slides") {
    return toList(["ggs"]);
  } else if (mime_type === "application/vnd.geogebra.tool") {
    return toList(["ggt"]);
  } else if (mime_type === "application/vnd.geometry-explorer") {
    return toList(["gex", "gre"]);
  } else if (mime_type === "application/vnd.geonext") {
    return toList(["gxt"]);
  } else if (mime_type === "application/vnd.geoplan") {
    return toList(["g2w"]);
  } else if (mime_type === "application/vnd.geospace") {
    return toList(["g3w"]);
  } else if (mime_type === "application/vnd.gmx") {
    return toList(["gmx"]);
  } else if (mime_type === "application/vnd.google-earth.kml+xml") {
    return toList(["kml"]);
  } else if (mime_type === "application/vnd.google-earth.kmz") {
    return toList(["kmz"]);
  } else if (mime_type === "application/vnd.grafeq") {
    return toList(["gqf", "gqs"]);
  } else if (mime_type === "application/vnd.groove-account") {
    return toList(["gac"]);
  } else if (mime_type === "application/vnd.groove-help") {
    return toList(["ghf"]);
  } else if (mime_type === "application/vnd.groove-identity-message") {
    return toList(["gim"]);
  } else if (mime_type === "application/vnd.groove-injector") {
    return toList(["grv"]);
  } else if (mime_type === "application/vnd.groove-tool-message") {
    return toList(["gtm"]);
  } else if (mime_type === "application/vnd.groove-tool-template") {
    return toList(["tpl"]);
  } else if (mime_type === "application/vnd.groove-vcard") {
    return toList(["vcg"]);
  } else if (mime_type === "application/vnd.hal+xml") {
    return toList(["hal"]);
  } else if (mime_type === "application/vnd.handheld-entertainment+xml") {
    return toList(["zmm"]);
  } else if (mime_type === "application/vnd.hbci") {
    return toList(["hbci"]);
  } else if (mime_type === "application/vnd.hhe.lesson-player") {
    return toList(["les"]);
  } else if (mime_type === "application/vnd.hp-hpgl") {
    return toList(["hpgl"]);
  } else if (mime_type === "application/vnd.hp-hpid") {
    return toList(["hpid"]);
  } else if (mime_type === "application/vnd.hp-hps") {
    return toList(["hps"]);
  } else if (mime_type === "application/vnd.hp-jlyt") {
    return toList(["jlt"]);
  } else if (mime_type === "application/vnd.hp-pcl") {
    return toList(["pcl"]);
  } else if (mime_type === "application/vnd.hp-pclxl") {
    return toList(["pclxl"]);
  } else if (mime_type === "application/vnd.hydrostatix.sof-data") {
    return toList(["sfd-hdstx"]);
  } else if (mime_type === "application/vnd.ibm.minipay") {
    return toList(["mpy"]);
  } else if (mime_type === "application/vnd.ibm.modcap") {
    return toList(["afp", "listafp", "list3820"]);
  } else if (mime_type === "application/vnd.ibm.rights-management") {
    return toList(["irm"]);
  } else if (mime_type === "application/vnd.ibm.secure-container") {
    return toList(["sc"]);
  } else if (mime_type === "application/vnd.iccprofile") {
    return toList(["icc", "icm"]);
  } else if (mime_type === "application/vnd.igloader") {
    return toList(["igl"]);
  } else if (mime_type === "application/vnd.immervision-ivp") {
    return toList(["ivp"]);
  } else if (mime_type === "application/vnd.immervision-ivu") {
    return toList(["ivu"]);
  } else if (mime_type === "application/vnd.insors.igm") {
    return toList(["igm"]);
  } else if (mime_type === "application/vnd.intercon.formnet") {
    return toList(["xpw", "xpx"]);
  } else if (mime_type === "application/vnd.intergeo") {
    return toList(["i2g"]);
  } else if (mime_type === "application/vnd.intu.qbo") {
    return toList(["qbo"]);
  } else if (mime_type === "application/vnd.intu.qfx") {
    return toList(["qfx"]);
  } else if (mime_type === "application/vnd.ipunplugged.rcprofile") {
    return toList(["rcprofile"]);
  } else if (mime_type === "application/vnd.irepository.package+xml") {
    return toList(["irp"]);
  } else if (mime_type === "application/vnd.is-xpr") {
    return toList(["xpr"]);
  } else if (mime_type === "application/vnd.isac.fcs") {
    return toList(["fcs"]);
  } else if (mime_type === "application/vnd.jam") {
    return toList(["jam"]);
  } else if (mime_type === "application/vnd.jcp.javame.midlet-rms") {
    return toList(["rms"]);
  } else if (mime_type === "application/vnd.jisp") {
    return toList(["jisp"]);
  } else if (mime_type === "application/vnd.joost.joda-archive") {
    return toList(["joda"]);
  } else if (mime_type === "application/vnd.kahootz") {
    return toList(["ktz", "ktr"]);
  } else if (mime_type === "application/vnd.kde.karbon") {
    return toList(["karbon"]);
  } else if (mime_type === "application/vnd.kde.kchart") {
    return toList(["chrt"]);
  } else if (mime_type === "application/vnd.kde.kformula") {
    return toList(["kfo"]);
  } else if (mime_type === "application/vnd.kde.kivio") {
    return toList(["flw"]);
  } else if (mime_type === "application/vnd.kde.kontour") {
    return toList(["kon"]);
  } else if (mime_type === "application/vnd.kde.kpresenter") {
    return toList(["kpr", "kpt"]);
  } else if (mime_type === "application/vnd.kde.kspread") {
    return toList(["ksp"]);
  } else if (mime_type === "application/vnd.kde.kword") {
    return toList(["kwd", "kwt"]);
  } else if (mime_type === "application/vnd.kenameaapp") {
    return toList(["htke"]);
  } else if (mime_type === "application/vnd.kidspiration") {
    return toList(["kia"]);
  } else if (mime_type === "application/vnd.kinar") {
    return toList(["kne", "knp"]);
  } else if (mime_type === "application/vnd.koan") {
    return toList(["skp", "skd", "skt", "skm"]);
  } else if (mime_type === "application/vnd.kodak-descriptor") {
    return toList(["sse"]);
  } else if (mime_type === "application/vnd.las.las+xml") {
    return toList(["lasxml"]);
  } else if (mime_type === "application/vnd.llamagraphics.life-balance.desktop") {
    return toList(["lbd"]);
  } else if (
    mime_type === "application/vnd.llamagraphics.life-balance.exchange+xml"
  ) {
    return toList(["lbe"]);
  } else if (mime_type === "application/vnd.lotus-1-2-3") {
    return toList(["123"]);
  } else if (mime_type === "application/vnd.lotus-approach") {
    return toList(["apr"]);
  } else if (mime_type === "application/vnd.lotus-freelance") {
    return toList(["pre"]);
  } else if (mime_type === "application/vnd.lotus-notes") {
    return toList(["nsf"]);
  } else if (mime_type === "application/vnd.lotus-organizer") {
    return toList(["org"]);
  } else if (mime_type === "application/vnd.lotus-screencam") {
    return toList(["scm"]);
  } else if (mime_type === "application/vnd.lotus-wordpro") {
    return toList(["lwp"]);
  } else if (mime_type === "application/vnd.macports.portpkg") {
    return toList(["portpkg"]);
  } else if (mime_type === "application/vnd.mcd") {
    return toList(["mcd"]);
  } else if (mime_type === "application/vnd.medcalcdata") {
    return toList(["mc1"]);
  } else if (mime_type === "application/vnd.mediastation.cdkey") {
    return toList(["cdkey"]);
  } else if (mime_type === "application/vnd.mfer") {
    return toList(["mwf"]);
  } else if (mime_type === "application/vnd.mfmp") {
    return toList(["mfm"]);
  } else if (mime_type === "application/vnd.micrografx.flo") {
    return toList(["flo"]);
  } else if (mime_type === "application/vnd.micrografx.igx") {
    return toList(["igx"]);
  } else if (mime_type === "application/vnd.mif") {
    return toList(["mif"]);
  } else if (mime_type === "application/vnd.mobius.daf") {
    return toList(["daf"]);
  } else if (mime_type === "application/vnd.mobius.dis") {
    return toList(["dis"]);
  } else if (mime_type === "application/vnd.mobius.mbk") {
    return toList(["mbk"]);
  } else if (mime_type === "application/vnd.mobius.mqy") {
    return toList(["mqy"]);
  } else if (mime_type === "application/vnd.mobius.msl") {
    return toList(["msl"]);
  } else if (mime_type === "application/vnd.mobius.plc") {
    return toList(["plc"]);
  } else if (mime_type === "application/vnd.mobius.txf") {
    return toList(["txf"]);
  } else if (mime_type === "application/vnd.mophun.application") {
    return toList(["mpn"]);
  } else if (mime_type === "application/vnd.mophun.certificate") {
    return toList(["mpc"]);
  } else if (mime_type === "application/vnd.mozilla.xul+xml") {
    return toList(["xul"]);
  } else if (mime_type === "application/vnd.ms-artgalry") {
    return toList(["cil"]);
  } else if (mime_type === "application/vnd.ms-cab-compressed") {
    return toList(["cab"]);
  } else if (mime_type === "application/vnd.ms-excel") {
    return toList(["xls", "xlm", "xla", "xlc", "xlt", "xlw"]);
  } else if (mime_type === "application/vnd.ms-excel.addin.macroenabled.12") {
    return toList(["xlam"]);
  } else if (
    mime_type === "application/vnd.ms-excel.sheet.binary.macroenabled.12"
  ) {
    return toList(["xlsb"]);
  } else if (mime_type === "application/vnd.ms-excel.sheet.macroenabled.12") {
    return toList(["xlsm"]);
  } else if (mime_type === "application/vnd.ms-excel.template.macroenabled.12") {
    return toList(["xltm"]);
  } else if (mime_type === "application/vnd.ms-fontobject") {
    return toList(["eot"]);
  } else if (mime_type === "application/vnd.ms-htmlhelp") {
    return toList(["chm"]);
  } else if (mime_type === "application/vnd.ms-ims") {
    return toList(["ims"]);
  } else if (mime_type === "application/vnd.ms-lrm") {
    return toList(["lrm"]);
  } else if (mime_type === "application/vnd.ms-officetheme") {
    return toList(["thmx"]);
  } else if (mime_type === "application/vnd.ms-pki.seccat") {
    return toList(["cat"]);
  } else if (mime_type === "application/vnd.ms-pki.stl") {
    return toList(["stl"]);
  } else if (mime_type === "application/vnd.ms-powerpoint") {
    return toList(["ppt", "pps", "pot"]);
  } else if (mime_type === "application/vnd.ms-powerpoint.addin.macroenabled.12") {
    return toList(["ppam"]);
  } else if (
    mime_type === "application/vnd.ms-powerpoint.presentation.macroenabled.12"
  ) {
    return toList(["pptm"]);
  } else if (mime_type === "application/vnd.ms-powerpoint.slide.macroenabled.12") {
    return toList(["sldm"]);
  } else if (
    mime_type === "application/vnd.ms-powerpoint.slideshow.macroenabled.12"
  ) {
    return toList(["ppsm"]);
  } else if (
    mime_type === "application/vnd.ms-powerpoint.template.macroenabled.12"
  ) {
    return toList(["potm"]);
  } else if (mime_type === "application/vnd.ms-project") {
    return toList(["mpp", "mpt"]);
  } else if (mime_type === "application/vnd.ms-word.document.macroenabled.12") {
    return toList(["docm"]);
  } else if (mime_type === "application/vnd.ms-word.template.macroenabled.12") {
    return toList(["dotm"]);
  } else if (mime_type === "application/vnd.ms-works") {
    return toList(["wps", "wks", "wcm", "wdb"]);
  } else if (mime_type === "application/vnd.ms-wpl") {
    return toList(["wpl"]);
  } else if (mime_type === "application/vnd.ms-xpsdocument") {
    return toList(["xps"]);
  } else if (mime_type === "application/vnd.mseq") {
    return toList(["mseq"]);
  } else if (mime_type === "application/vnd.musician") {
    return toList(["mus"]);
  } else if (mime_type === "application/vnd.muvee.style") {
    return toList(["msty"]);
  } else if (mime_type === "application/vnd.mynfc") {
    return toList(["taglet"]);
  } else if (mime_type === "application/vnd.neurolanguage.nlu") {
    return toList(["nlu"]);
  } else if (mime_type === "application/vnd.nitf") {
    return toList(["ntf", "nitf"]);
  } else if (mime_type === "application/vnd.noblenet-directory") {
    return toList(["nnd"]);
  } else if (mime_type === "application/vnd.noblenet-sealer") {
    return toList(["nns"]);
  } else if (mime_type === "application/vnd.noblenet-web") {
    return toList(["nnw"]);
  } else if (mime_type === "application/vnd.nokia.n-gage.data") {
    return toList(["ngdat"]);
  } else if (mime_type === "application/vnd.nokia.n-gage.symbian.install") {
    return toList(["n-gage"]);
  } else if (mime_type === "application/vnd.nokia.radio-preset") {
    return toList(["rpst"]);
  } else if (mime_type === "application/vnd.nokia.radio-presets") {
    return toList(["rpss"]);
  } else if (mime_type === "application/vnd.novadigm.edm") {
    return toList(["edm"]);
  } else if (mime_type === "application/vnd.novadigm.edx") {
    return toList(["edx"]);
  } else if (mime_type === "application/vnd.novadigm.ext") {
    return toList(["ext"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.chart") {
    return toList(["odc"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.chart-template") {
    return toList(["otc"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.database") {
    return toList(["odb"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.formula") {
    return toList(["odf"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.formula-template") {
    return toList(["odft"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.graphics") {
    return toList(["odg"]);
  } else if (
    mime_type === "application/vnd.oasis.opendocument.graphics-template"
  ) {
    return toList(["otg"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.image") {
    return toList(["odi"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.image-template") {
    return toList(["oti"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.presentation") {
    return toList(["odp"]);
  } else if (
    mime_type === "application/vnd.oasis.opendocument.presentation-template"
  ) {
    return toList(["otp"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.spreadsheet") {
    return toList(["ods"]);
  } else if (
    mime_type === "application/vnd.oasis.opendocument.spreadsheet-template"
  ) {
    return toList(["ots"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.text") {
    return toList(["odt"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.text-master") {
    return toList(["odm"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.text-template") {
    return toList(["ott"]);
  } else if (mime_type === "application/vnd.oasis.opendocument.text-web") {
    return toList(["oth"]);
  } else if (mime_type === "application/vnd.olpc-sugar") {
    return toList(["xo"]);
  } else if (mime_type === "application/vnd.oma.dd2+xml") {
    return toList(["dd2"]);
  } else if (mime_type === "application/vnd.openofficeorg.extension") {
    return toList(["oxt"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  ) {
    return toList(["pptx"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.presentationml.slide"
  ) {
    return toList(["sldx"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.presentationml.slideshow"
  ) {
    return toList(["ppsx"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.presentationml.template"
  ) {
    return toList(["potx"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  ) {
    return toList(["xlsx"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.spreadsheetml.template"
  ) {
    return toList(["xltx"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  ) {
    return toList(["docx"]);
  } else if (
    mime_type === "application/vnd.openxmlformats-officedocument.wordprocessingml.template"
  ) {
    return toList(["dotx"]);
  } else if (mime_type === "application/vnd.osgeo.mapguide.package") {
    return toList(["mgp"]);
  } else if (mime_type === "application/vnd.osgi.dp") {
    return toList(["dp"]);
  } else if (mime_type === "application/vnd.osgi.subsystem") {
    return toList(["esa"]);
  } else if (mime_type === "application/vnd.palm") {
    return toList(["pdb", "pqa", "oprc"]);
  } else if (mime_type === "application/vnd.pawaafile") {
    return toList(["paw"]);
  } else if (mime_type === "application/vnd.pg.format") {
    return toList(["str"]);
  } else if (mime_type === "application/vnd.pg.osasli") {
    return toList(["ei6"]);
  } else if (mime_type === "application/vnd.picsel") {
    return toList(["efif"]);
  } else if (mime_type === "application/vnd.pmi.widget") {
    return toList(["wg"]);
  } else if (mime_type === "application/vnd.pocketlearn") {
    return toList(["plf"]);
  } else if (mime_type === "application/vnd.powerbuilder6") {
    return toList(["pbd"]);
  } else if (mime_type === "application/vnd.previewsystems.box") {
    return toList(["box"]);
  } else if (mime_type === "application/vnd.proteus.magazine") {
    return toList(["mgz"]);
  } else if (mime_type === "application/vnd.publishare-delta-tree") {
    return toList(["qps"]);
  } else if (mime_type === "application/vnd.pvi.ptid1") {
    return toList(["ptid"]);
  } else if (mime_type === "application/vnd.quark.quarkxpress") {
    return toList(["qxd", "qxt", "qwd", "qwt", "qxl", "qxb"]);
  } else if (mime_type === "application/vnd.realvnc.bed") {
    return toList(["bed"]);
  } else if (mime_type === "application/vnd.recordare.musicxml") {
    return toList(["mxl"]);
  } else if (mime_type === "application/vnd.recordare.musicxml+xml") {
    return toList(["musicxml"]);
  } else if (mime_type === "application/vnd.rig.cryptonote") {
    return toList(["cryptonote"]);
  } else if (mime_type === "application/vnd.rim.cod") {
    return toList(["cod"]);
  } else if (mime_type === "application/vnd.rn-realmedia") {
    return toList(["rm"]);
  } else if (mime_type === "application/vnd.rn-realmedia-vbr") {
    return toList(["rmvb"]);
  } else if (mime_type === "application/vnd.route66.link66+xml") {
    return toList(["link66"]);
  } else if (mime_type === "application/vnd.sailingtracker.track") {
    return toList(["st"]);
  } else if (mime_type === "application/vnd.seemail") {
    return toList(["see"]);
  } else if (mime_type === "application/vnd.sema") {
    return toList(["sema"]);
  } else if (mime_type === "application/vnd.semd") {
    return toList(["semd"]);
  } else if (mime_type === "application/vnd.semf") {
    return toList(["semf"]);
  } else if (mime_type === "application/vnd.shana.informed.formdata") {
    return toList(["ifm"]);
  } else if (mime_type === "application/vnd.shana.informed.formtemplate") {
    return toList(["itp"]);
  } else if (mime_type === "application/vnd.shana.informed.interchange") {
    return toList(["iif"]);
  } else if (mime_type === "application/vnd.shana.informed.package") {
    return toList(["ipk"]);
  } else if (mime_type === "application/vnd.simtech-mindmapper") {
    return toList(["twd", "twds"]);
  } else if (mime_type === "application/vnd.smaf") {
    return toList(["mmf"]);
  } else if (mime_type === "application/vnd.smart.teacher") {
    return toList(["teacher"]);
  } else if (mime_type === "application/vnd.solent.sdkm+xml") {
    return toList(["sdkm", "sdkd"]);
  } else if (mime_type === "application/vnd.spotfire.dxp") {
    return toList(["dxp"]);
  } else if (mime_type === "application/vnd.spotfire.sfs") {
    return toList(["sfs"]);
  } else if (mime_type === "application/vnd.stardivision.calc") {
    return toList(["sdc"]);
  } else if (mime_type === "application/vnd.stardivision.draw") {
    return toList(["sda"]);
  } else if (mime_type === "application/vnd.stardivision.impress") {
    return toList(["sdd"]);
  } else if (mime_type === "application/vnd.stardivision.math") {
    return toList(["smf"]);
  } else if (mime_type === "application/vnd.stardivision.writer") {
    return toList(["sdw", "vor"]);
  } else if (mime_type === "application/vnd.stardivision.writer-global") {
    return toList(["sgl"]);
  } else if (mime_type === "application/vnd.stepmania.package") {
    return toList(["smzip"]);
  } else if (mime_type === "application/vnd.stepmania.stepchart") {
    return toList(["sm"]);
  } else if (mime_type === "application/vnd.sun.xml.calc") {
    return toList(["sxc"]);
  } else if (mime_type === "application/vnd.sun.xml.calc.template") {
    return toList(["stc"]);
  } else if (mime_type === "application/vnd.sun.xml.draw") {
    return toList(["sxd"]);
  } else if (mime_type === "application/vnd.sun.xml.draw.template") {
    return toList(["std"]);
  } else if (mime_type === "application/vnd.sun.xml.impress") {
    return toList(["sxi"]);
  } else if (mime_type === "application/vnd.sun.xml.impress.template") {
    return toList(["sti"]);
  } else if (mime_type === "application/vnd.sun.xml.math") {
    return toList(["sxm"]);
  } else if (mime_type === "application/vnd.sun.xml.writer") {
    return toList(["sxw"]);
  } else if (mime_type === "application/vnd.sun.xml.writer.global") {
    return toList(["sxg"]);
  } else if (mime_type === "application/vnd.sun.xml.writer.template") {
    return toList(["stw"]);
  } else if (mime_type === "application/vnd.sus-calendar") {
    return toList(["sus", "susp"]);
  } else if (mime_type === "application/vnd.svd") {
    return toList(["svd"]);
  } else if (mime_type === "application/vnd.symbian.install") {
    return toList(["sis", "sisx"]);
  } else if (mime_type === "application/vnd.syncml+xml") {
    return toList(["xsm"]);
  } else if (mime_type === "application/vnd.syncml.dm+wbxml") {
    return toList(["bdm"]);
  } else if (mime_type === "application/vnd.syncml.dm+xml") {
    return toList(["xdm"]);
  } else if (mime_type === "application/vnd.tao.intent-module-archive") {
    return toList(["tao"]);
  } else if (mime_type === "application/vnd.tcpdump.pcap") {
    return toList(["pcap", "cap", "dmp"]);
  } else if (mime_type === "application/vnd.tmobile-livetv") {
    return toList(["tmo"]);
  } else if (mime_type === "application/vnd.trid.tpt") {
    return toList(["tpt"]);
  } else if (mime_type === "application/vnd.triscape.mxs") {
    return toList(["mxs"]);
  } else if (mime_type === "application/vnd.trueapp") {
    return toList(["tra"]);
  } else if (mime_type === "application/vnd.ufdl") {
    return toList(["ufd", "ufdl"]);
  } else if (mime_type === "application/vnd.uiq.theme") {
    return toList(["utz"]);
  } else if (mime_type === "application/vnd.umajin") {
    return toList(["umj"]);
  } else if (mime_type === "application/vnd.unity") {
    return toList(["unityweb"]);
  } else if (mime_type === "application/vnd.uoml+xml") {
    return toList(["uoml"]);
  } else if (mime_type === "application/vnd.vcx") {
    return toList(["vcx"]);
  } else if (mime_type === "application/vnd.visio") {
    return toList(["vsd", "vst", "vss", "vsw"]);
  } else if (mime_type === "application/vnd.visionary") {
    return toList(["vis"]);
  } else if (mime_type === "application/vnd.vsf") {
    return toList(["vsf"]);
  } else if (mime_type === "application/vnd.wap.wbxml") {
    return toList(["wbxml"]);
  } else if (mime_type === "application/vnd.wap.wmlc") {
    return toList(["wmlc"]);
  } else if (mime_type === "application/vnd.wap.wmlscriptc") {
    return toList(["wmlsc"]);
  } else if (mime_type === "application/vnd.webturbo") {
    return toList(["wtb"]);
  } else if (mime_type === "application/vnd.wolfram.player") {
    return toList(["nbp"]);
  } else if (mime_type === "application/vnd.wordperfect") {
    return toList(["wpd"]);
  } else if (mime_type === "application/vnd.wqd") {
    return toList(["wqd"]);
  } else if (mime_type === "application/vnd.wt.stf") {
    return toList(["stf"]);
  } else if (mime_type === "application/vnd.xara") {
    return toList(["xar"]);
  } else if (mime_type === "application/vnd.xfdl") {
    return toList(["xfdl"]);
  } else if (mime_type === "application/vnd.yamaha.hv-dic") {
    return toList(["hvd"]);
  } else if (mime_type === "application/vnd.yamaha.hv-script") {
    return toList(["hvs"]);
  } else if (mime_type === "application/vnd.yamaha.hv-voice") {
    return toList(["hvp"]);
  } else if (mime_type === "application/vnd.yamaha.openscoreformat") {
    return toList(["osf"]);
  } else if (mime_type === "application/vnd.yamaha.openscoreformat.osfpvg+xml") {
    return toList(["osfpvg"]);
  } else if (mime_type === "application/vnd.yamaha.smaf-audio") {
    return toList(["saf"]);
  } else if (mime_type === "application/vnd.yamaha.smaf-phrase") {
    return toList(["spf"]);
  } else if (mime_type === "application/vnd.yellowriver-custom-menu") {
    return toList(["cmp"]);
  } else if (mime_type === "application/vnd.zul") {
    return toList(["zir", "zirz"]);
  } else if (mime_type === "application/vnd.zzazz.deck+xml") {
    return toList(["zaz"]);
  } else if (mime_type === "application/voicexml+xml") {
    return toList(["vxml"]);
  } else if (mime_type === "application/wasm") {
    return toList(["wasm"]);
  } else if (mime_type === "application/widget") {
    return toList(["wgt"]);
  } else if (mime_type === "application/winhlp") {
    return toList(["hlp"]);
  } else if (mime_type === "application/wsdl+xml") {
    return toList(["wsdl"]);
  } else if (mime_type === "application/wspolicy+xml") {
    return toList(["wspolicy"]);
  } else if (mime_type === "application/x-7z-compressed") {
    return toList(["7z"]);
  } else if (mime_type === "application/x-abiword") {
    return toList(["abw"]);
  } else if (mime_type === "application/x-ace-compressed") {
    return toList(["ace"]);
  } else if (mime_type === "application/x-apple-diskimage") {
    return toList(["dmg"]);
  } else if (mime_type === "application/x-authorware-bin") {
    return toList(["aab", "x32", "u32", "vox"]);
  } else if (mime_type === "application/x-authorware-map") {
    return toList(["aam"]);
  } else if (mime_type === "application/x-authorware-seg") {
    return toList(["aas"]);
  } else if (mime_type === "application/x-bcpio") {
    return toList(["bcpio"]);
  } else if (mime_type === "application/x-bittorrent") {
    return toList(["torrent"]);
  } else if (mime_type === "application/x-blorb") {
    return toList(["blb", "blorb"]);
  } else if (mime_type === "application/x-bzip") {
    return toList(["bz"]);
  } else if (mime_type === "application/x-bzip2") {
    return toList(["bz2", "boz"]);
  } else if (mime_type === "application/x-cbr") {
    return toList(["cbr", "cba", "cbt", "cbz", "cb7"]);
  } else if (mime_type === "application/x-cdlink") {
    return toList(["vcd"]);
  } else if (mime_type === "application/x-cfs-compressed") {
    return toList(["cfs"]);
  } else if (mime_type === "application/x-chat") {
    return toList(["chat"]);
  } else if (mime_type === "application/x-chess-pgn") {
    return toList(["pgn"]);
  } else if (mime_type === "application/x-conference") {
    return toList(["nsc"]);
  } else if (mime_type === "application/x-cpio") {
    return toList(["cpio"]);
  } else if (mime_type === "application/x-csh") {
    return toList(["csh"]);
  } else if (mime_type === "application/x-debian-package") {
    return toList(["deb", "udeb"]);
  } else if (mime_type === "application/x-dgc-compressed") {
    return toList(["dgc"]);
  } else if (mime_type === "application/x-director") {
    return toList([
      "dir",
      "dcr",
      "dxr",
      "cst",
      "cct",
      "cxt",
      "w3d",
      "fgd",
      "swa",
    ]);
  } else if (mime_type === "application/x-doom") {
    return toList(["wad"]);
  } else if (mime_type === "application/x-dtbncx+xml") {
    return toList(["ncx"]);
  } else if (mime_type === "application/x-dtbook+xml") {
    return toList(["dtb"]);
  } else if (mime_type === "application/x-dtbresource+xml") {
    return toList(["res"]);
  } else if (mime_type === "application/x-dvi") {
    return toList(["dvi"]);
  } else if (mime_type === "application/x-envoy") {
    return toList(["evy"]);
  } else if (mime_type === "application/x-eva") {
    return toList(["eva"]);
  } else if (mime_type === "application/x-font-bdf") {
    return toList(["bdf"]);
  } else if (mime_type === "application/x-font-ghostscript") {
    return toList(["gsf"]);
  } else if (mime_type === "application/x-font-linux-psf") {
    return toList(["psf"]);
  } else if (mime_type === "application/x-font-pcf") {
    return toList(["pcf"]);
  } else if (mime_type === "application/x-font-snf") {
    return toList(["snf"]);
  } else if (mime_type === "application/x-font-type1") {
    return toList(["pfa", "pfb", "pfm", "afm"]);
  } else if (mime_type === "application/x-freearc") {
    return toList(["arc"]);
  } else if (mime_type === "application/x-futuresplash") {
    return toList(["spl"]);
  } else if (mime_type === "application/x-gca-compressed") {
    return toList(["gca"]);
  } else if (mime_type === "application/x-glulx") {
    return toList(["ulx"]);
  } else if (mime_type === "application/x-gnumeric") {
    return toList(["gnumeric"]);
  } else if (mime_type === "application/x-gramps-xml") {
    return toList(["gramps"]);
  } else if (mime_type === "application/x-gtar") {
    return toList(["gtar"]);
  } else if (mime_type === "application/x-hdf") {
    return toList(["hdf"]);
  } else if (mime_type === "application/x-install-instructions") {
    return toList(["install"]);
  } else if (mime_type === "application/x-iso9660-image") {
    return toList(["iso"]);
  } else if (mime_type === "application/x-java-jnlp-file") {
    return toList(["jnlp"]);
  } else if (mime_type === "application/x-latex") {
    return toList(["latex"]);
  } else if (mime_type === "application/x-lzh-compressed") {
    return toList(["lzh", "lha"]);
  } else if (mime_type === "application/x-mie") {
    return toList(["mie"]);
  } else if (mime_type === "application/x-mobipocket-ebook") {
    return toList(["prc", "mobi"]);
  } else if (mime_type === "application/x-ms-application") {
    return toList(["application"]);
  } else if (mime_type === "application/x-ms-shortcut") {
    return toList(["lnk"]);
  } else if (mime_type === "application/x-ms-wmd") {
    return toList(["wmd"]);
  } else if (mime_type === "application/x-ms-wmz") {
    return toList(["wmz"]);
  } else if (mime_type === "application/x-ms-xbap") {
    return toList(["xbap"]);
  } else if (mime_type === "application/x-msaccess") {
    return toList(["mdb"]);
  } else if (mime_type === "application/x-msbinder") {
    return toList(["obd"]);
  } else if (mime_type === "application/x-mscardfile") {
    return toList(["crd"]);
  } else if (mime_type === "application/x-msclip") {
    return toList(["clp"]);
  } else if (mime_type === "application/x-msdownload") {
    return toList(["exe", "dll", "com", "bat", "msi"]);
  } else if (mime_type === "application/x-msmediaview") {
    return toList(["mvb", "m13", "m14"]);
  } else if (mime_type === "application/x-msmetafile") {
    return toList(["wmf", "wmz", "emf", "emz"]);
  } else if (mime_type === "application/x-msmoney") {
    return toList(["mny"]);
  } else if (mime_type === "application/x-mspublisher") {
    return toList(["pub"]);
  } else if (mime_type === "application/x-msschedule") {
    return toList(["scd"]);
  } else if (mime_type === "application/x-msterminal") {
    return toList(["trm"]);
  } else if (mime_type === "application/x-mswrite") {
    return toList(["wri"]);
  } else if (mime_type === "application/x-netcdf") {
    return toList(["nc", "cdf"]);
  } else if (mime_type === "application/x-nzb") {
    return toList(["nzb"]);
  } else if (mime_type === "application/x-pkcs12") {
    return toList(["p12", "pfx"]);
  } else if (mime_type === "application/x-pkcs7-certificates") {
    return toList(["p7b", "spc"]);
  } else if (mime_type === "application/x-pkcs7-certreqresp") {
    return toList(["p7r"]);
  } else if (mime_type === "application/x-rar-compressed") {
    return toList(["rar"]);
  } else if (mime_type === "application/x-research-info-systems") {
    return toList(["ris"]);
  } else if (mime_type === "application/x-sh") {
    return toList(["sh"]);
  } else if (mime_type === "application/x-shar") {
    return toList(["shar"]);
  } else if (mime_type === "application/x-shockwave-flash") {
    return toList(["swf"]);
  } else if (mime_type === "application/x-silverlight-app") {
    return toList(["xap"]);
  } else if (mime_type === "application/x-sql") {
    return toList(["sql"]);
  } else if (mime_type === "application/x-stuffit") {
    return toList(["sit"]);
  } else if (mime_type === "application/x-stuffitx") {
    return toList(["sitx"]);
  } else if (mime_type === "application/x-subrip") {
    return toList(["srt"]);
  } else if (mime_type === "application/x-sv4cpio") {
    return toList(["sv4cpio"]);
  } else if (mime_type === "application/x-sv4crc") {
    return toList(["sv4crc"]);
  } else if (mime_type === "application/x-t3vm-image") {
    return toList(["t3"]);
  } else if (mime_type === "application/x-tads") {
    return toList(["gam"]);
  } else if (mime_type === "application/x-tar") {
    return toList(["tar"]);
  } else if (mime_type === "application/x-tcl") {
    return toList(["tcl"]);
  } else if (mime_type === "application/x-tex") {
    return toList(["tex"]);
  } else if (mime_type === "application/x-tex-tfm") {
    return toList(["tfm"]);
  } else if (mime_type === "application/x-texinfo") {
    return toList(["texinfo", "texi"]);
  } else if (mime_type === "application/x-tgif") {
    return toList(["obj"]);
  } else if (mime_type === "application/x-ustar") {
    return toList(["ustar"]);
  } else if (mime_type === "application/x-wais-source") {
    return toList(["src"]);
  } else if (mime_type === "application/x-x509-ca-cert") {
    return toList(["der", "crt"]);
  } else if (mime_type === "application/x-xfig") {
    return toList(["fig"]);
  } else if (mime_type === "application/x-xliff+xml") {
    return toList(["xlf"]);
  } else if (mime_type === "application/x-xpinstall") {
    return toList(["xpi"]);
  } else if (mime_type === "application/x-xz") {
    return toList(["xz"]);
  } else if (mime_type === "application/x-zmachine") {
    return toList(["z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8"]);
  } else if (mime_type === "application/xaml+xml") {
    return toList(["xaml"]);
  } else if (mime_type === "application/xcap-diff+xml") {
    return toList(["xdf"]);
  } else if (mime_type === "application/xenc+xml") {
    return toList(["xenc"]);
  } else if (mime_type === "application/xhtml+xml") {
    return toList(["xhtml", "xht"]);
  } else if (mime_type === "application/xml") {
    return toList(["xml", "xsl"]);
  } else if (mime_type === "application/xml-dtd") {
    return toList(["dtd"]);
  } else if (mime_type === "application/xop+xml") {
    return toList(["xop"]);
  } else if (mime_type === "application/xproc+xml") {
    return toList(["xpl"]);
  } else if (mime_type === "application/xslt+xml") {
    return toList(["xslt"]);
  } else if (mime_type === "application/xspf+xml") {
    return toList(["xspf"]);
  } else if (mime_type === "application/xv+xml") {
    return toList(["mxml", "xhvml", "xvml", "xvm"]);
  } else if (mime_type === "application/yang") {
    return toList(["yang"]);
  } else if (mime_type === "application/yin+xml") {
    return toList(["yin"]);
  } else if (mime_type === "application/zip") {
    return toList(["zip"]);
  } else if (mime_type === "audio/adpcm") {
    return toList(["adp"]);
  } else if (mime_type === "audio/basic") {
    return toList(["au", "snd"]);
  } else if (mime_type === "audio/midi") {
    return toList(["mid", "midi", "kar", "rmi"]);
  } else if (mime_type === "audio/mp4") {
    return toList(["m4a", "mp4a"]);
  } else if (mime_type === "audio/mpeg") {
    return toList(["mpga", "mp2", "mp2a", "mp3", "m2a", "m3a"]);
  } else if (mime_type === "audio/ogg") {
    return toList(["oga", "ogg", "spx", "opus"]);
  } else if (mime_type === "audio/s3m") {
    return toList(["s3m"]);
  } else if (mime_type === "audio/silk") {
    return toList(["sil"]);
  } else if (mime_type === "audio/vnd.dece.audio") {
    return toList(["uva", "uvva"]);
  } else if (mime_type === "audio/vnd.digital-winds") {
    return toList(["eol"]);
  } else if (mime_type === "audio/vnd.dra") {
    return toList(["dra"]);
  } else if (mime_type === "audio/vnd.dts") {
    return toList(["dts"]);
  } else if (mime_type === "audio/vnd.dts.hd") {
    return toList(["dtshd"]);
  } else if (mime_type === "audio/vnd.lucent.voice") {
    return toList(["lvp"]);
  } else if (mime_type === "audio/vnd.ms-playready.media.pya") {
    return toList(["pya"]);
  } else if (mime_type === "audio/vnd.nuera.ecelp4800") {
    return toList(["ecelp4800"]);
  } else if (mime_type === "audio/vnd.nuera.ecelp7470") {
    return toList(["ecelp7470"]);
  } else if (mime_type === "audio/vnd.nuera.ecelp9600") {
    return toList(["ecelp9600"]);
  } else if (mime_type === "audio/vnd.rip") {
    return toList(["rip"]);
  } else if (mime_type === "audio/webm") {
    return toList(["weba"]);
  } else if (mime_type === "audio/x-aac") {
    return toList(["aac"]);
  } else if (mime_type === "audio/x-aiff") {
    return toList(["aif", "aiff", "aifc"]);
  } else if (mime_type === "audio/x-caf") {
    return toList(["caf"]);
  } else if (mime_type === "audio/x-flac") {
    return toList(["flac"]);
  } else if (mime_type === "audio/x-matroska") {
    return toList(["mka"]);
  } else if (mime_type === "audio/x-mpegurl") {
    return toList(["m3u"]);
  } else if (mime_type === "audio/x-ms-wax") {
    return toList(["wax"]);
  } else if (mime_type === "audio/x-ms-wma") {
    return toList(["wma"]);
  } else if (mime_type === "audio/x-pn-realaudio") {
    return toList(["ram", "ra"]);
  } else if (mime_type === "audio/x-pn-realaudio-plugin") {
    return toList(["rmp"]);
  } else if (mime_type === "audio/x-wav") {
    return toList(["wav"]);
  } else if (mime_type === "audio/xm") {
    return toList(["xm"]);
  } else if (mime_type === "chemical/x-cdx") {
    return toList(["cdx"]);
  } else if (mime_type === "chemical/x-cif") {
    return toList(["cif"]);
  } else if (mime_type === "chemical/x-cmdf") {
    return toList(["cmdf"]);
  } else if (mime_type === "chemical/x-cml") {
    return toList(["cml"]);
  } else if (mime_type === "chemical/x-csml") {
    return toList(["csml"]);
  } else if (mime_type === "chemical/x-xyz") {
    return toList(["xyz"]);
  } else if (mime_type === "font/collection") {
    return toList(["ttc"]);
  } else if (mime_type === "font/otf") {
    return toList(["otf"]);
  } else if (mime_type === "font/ttf") {
    return toList(["ttf"]);
  } else if (mime_type === "font/woff") {
    return toList(["woff"]);
  } else if (mime_type === "font/woff2") {
    return toList(["woff2"]);
  } else if (mime_type === "image/avif") {
    return toList(["avif"]);
  } else if (mime_type === "image/bmp") {
    return toList(["bmp"]);
  } else if (mime_type === "image/cgm") {
    return toList(["cgm"]);
  } else if (mime_type === "image/g3fax") {
    return toList(["g3"]);
  } else if (mime_type === "image/gif") {
    return toList(["gif"]);
  } else if (mime_type === "image/ief") {
    return toList(["ief"]);
  } else if (mime_type === "image/jpeg") {
    return toList(["jpeg", "jpg", "jpe"]);
  } else if (mime_type === "image/jxl") {
    return toList(["jxl"]);
  } else if (mime_type === "image/ktx") {
    return toList(["ktx"]);
  } else if (mime_type === "image/png") {
    return toList(["png"]);
  } else if (mime_type === "image/prs.btif") {
    return toList(["btif"]);
  } else if (mime_type === "image/sgi") {
    return toList(["sgi"]);
  } else if (mime_type === "image/svg+xml") {
    return toList(["svg", "svgz"]);
  } else if (mime_type === "image/tiff") {
    return toList(["tiff", "tif"]);
  } else if (mime_type === "image/vnd.adobe.photoshop") {
    return toList(["psd"]);
  } else if (mime_type === "image/vnd.dece.graphic") {
    return toList(["uvi", "uvvi", "uvg", "uvvg"]);
  } else if (mime_type === "image/vnd.djvu") {
    return toList(["djvu", "djv"]);
  } else if (mime_type === "image/vnd.dvb.subtitle") {
    return toList(["sub"]);
  } else if (mime_type === "image/vnd.dwg") {
    return toList(["dwg"]);
  } else if (mime_type === "image/vnd.dxf") {
    return toList(["dxf"]);
  } else if (mime_type === "image/vnd.fastbidsheet") {
    return toList(["fbs"]);
  } else if (mime_type === "image/vnd.fpx") {
    return toList(["fpx"]);
  } else if (mime_type === "image/vnd.fst") {
    return toList(["fst"]);
  } else if (mime_type === "image/vnd.fujixerox.edmics-mmr") {
    return toList(["mmr"]);
  } else if (mime_type === "image/vnd.fujixerox.edmics-rlc") {
    return toList(["rlc"]);
  } else if (mime_type === "image/vnd.ms-modi") {
    return toList(["mdi"]);
  } else if (mime_type === "image/vnd.ms-photo") {
    return toList(["wdp"]);
  } else if (mime_type === "image/vnd.net-fpx") {
    return toList(["npx"]);
  } else if (mime_type === "image/vnd.wap.wbmp") {
    return toList(["wbmp"]);
  } else if (mime_type === "image/vnd.xiff") {
    return toList(["xif"]);
  } else if (mime_type === "image/webp") {
    return toList(["webp"]);
  } else if (mime_type === "image/x-3ds") {
    return toList(["3ds"]);
  } else if (mime_type === "image/x-cmu-raster") {
    return toList(["ras"]);
  } else if (mime_type === "image/x-cmx") {
    return toList(["cmx"]);
  } else if (mime_type === "image/x-freehand") {
    return toList(["fh", "fhc", "fh4", "fh5", "fh7"]);
  } else if (mime_type === "image/x-icon") {
    return toList(["ico"]);
  } else if (mime_type === "image/x-mrsid-image") {
    return toList(["sid"]);
  } else if (mime_type === "image/x-pcx") {
    return toList(["pcx"]);
  } else if (mime_type === "image/x-pict") {
    return toList(["pic", "pct"]);
  } else if (mime_type === "image/x-portable-anymap") {
    return toList(["pnm"]);
  } else if (mime_type === "image/x-portable-bitmap") {
    return toList(["pbm"]);
  } else if (mime_type === "image/x-portable-graymap") {
    return toList(["pgm"]);
  } else if (mime_type === "image/x-portable-pixmap") {
    return toList(["ppm"]);
  } else if (mime_type === "image/x-rgb") {
    return toList(["rgb"]);
  } else if (mime_type === "image/x-tga") {
    return toList(["tga"]);
  } else if (mime_type === "image/x-xbitmap") {
    return toList(["xbm"]);
  } else if (mime_type === "image/x-xpixmap") {
    return toList(["xpm"]);
  } else if (mime_type === "image/x-xwindowdump") {
    return toList(["xwd"]);
  } else if (mime_type === "message/rfc822") {
    return toList(["eml", "mime"]);
  } else if (mime_type === "model/iges") {
    return toList(["igs", "iges"]);
  } else if (mime_type === "model/mesh") {
    return toList(["msh", "mesh", "silo"]);
  } else if (mime_type === "model/vnd.collada+xml") {
    return toList(["dae"]);
  } else if (mime_type === "model/vnd.dwf") {
    return toList(["dwf"]);
  } else if (mime_type === "model/vnd.gdl") {
    return toList(["gdl"]);
  } else if (mime_type === "model/vnd.gtw") {
    return toList(["gtw"]);
  } else if (mime_type === "model/vnd.vtu") {
    return toList(["vtu"]);
  } else if (mime_type === "model/vrml") {
    return toList(["wrl", "vrml"]);
  } else if (mime_type === "model/x3d+binary") {
    return toList(["x3db", "x3dbz"]);
  } else if (mime_type === "model/x3d+vrml") {
    return toList(["x3dv", "x3dvz"]);
  } else if (mime_type === "model/x3d+xml") {
    return toList(["x3d", "x3dz"]);
  } else if (mime_type === "text/cache-manifest") {
    return toList(["appcache"]);
  } else if (mime_type === "text/calendar") {
    return toList(["ics", "ifb"]);
  } else if (mime_type === "text/css") {
    return toList(["css"]);
  } else if (mime_type === "text/csv") {
    return toList(["csv"]);
  } else if (mime_type === "text/html") {
    return toList(["html", "htm"]);
  } else if (mime_type === "text/javascript") {
    return toList(["js", "mjs"]);
  } else if (mime_type === "text/n3") {
    return toList(["n3"]);
  } else if (mime_type === "text/plain") {
    return toList(["txt", "text", "conf", "def", "list", "log", "in"]);
  } else if (mime_type === "text/prs.lines.tag") {
    return toList(["dsc"]);
  } else if (mime_type === "text/richtext") {
    return toList(["rtx"]);
  } else if (mime_type === "text/sgml") {
    return toList(["sgml", "sgm"]);
  } else if (mime_type === "text/tab-separated-values") {
    return toList(["tsv"]);
  } else if (mime_type === "text/troff") {
    return toList(["t", "tr", "roff", "man", "me", "ms"]);
  } else if (mime_type === "text/turtle") {
    return toList(["ttl"]);
  } else if (mime_type === "text/uri-list") {
    return toList(["uri", "uris", "urls"]);
  } else if (mime_type === "text/vcard") {
    return toList(["vcard"]);
  } else if (mime_type === "text/vnd.curl") {
    return toList(["curl"]);
  } else if (mime_type === "text/vnd.curl.dcurl") {
    return toList(["dcurl"]);
  } else if (mime_type === "text/vnd.curl.mcurl") {
    return toList(["mcurl"]);
  } else if (mime_type === "text/vnd.curl.scurl") {
    return toList(["scurl"]);
  } else if (mime_type === "text/vnd.dvb.subtitle") {
    return toList(["sub"]);
  } else if (mime_type === "text/vnd.fly") {
    return toList(["fly"]);
  } else if (mime_type === "text/vnd.fmi.flexstor") {
    return toList(["flx"]);
  } else if (mime_type === "text/vnd.graphviz") {
    return toList(["gv"]);
  } else if (mime_type === "text/vnd.in3d.3dml") {
    return toList(["3dml"]);
  } else if (mime_type === "text/vnd.in3d.spot") {
    return toList(["spot"]);
  } else if (mime_type === "text/vnd.sun.j2me.app-descriptor") {
    return toList(["jad"]);
  } else if (mime_type === "text/vnd.wap.wml") {
    return toList(["wml"]);
  } else if (mime_type === "text/vnd.wap.wmlscript") {
    return toList(["wmls"]);
  } else if (mime_type === "text/x-asm") {
    return toList(["s", "asm"]);
  } else if (mime_type === "text/x-c") {
    return toList(["c", "cc", "cxx", "cpp", "h", "hh", "dic"]);
  } else if (mime_type === "text/x-fortran") {
    return toList(["f", "for", "f77", "f90"]);
  } else if (mime_type === "text/x-java-source") {
    return toList(["java"]);
  } else if (mime_type === "text/x-nfo") {
    return toList(["nfo"]);
  } else if (mime_type === "text/x-opml") {
    return toList(["opml"]);
  } else if (mime_type === "text/x-pascal") {
    return toList(["p", "pas"]);
  } else if (mime_type === "text/x-setext") {
    return toList(["etx"]);
  } else if (mime_type === "text/x-sfv") {
    return toList(["sfv"]);
  } else if (mime_type === "text/x-uuencode") {
    return toList(["uu"]);
  } else if (mime_type === "text/x-vcalendar") {
    return toList(["vcs"]);
  } else if (mime_type === "text/x-vcard") {
    return toList(["vcf"]);
  } else if (mime_type === "video/3gpp") {
    return toList(["3gp"]);
  } else if (mime_type === "video/3gpp2") {
    return toList(["3g2"]);
  } else if (mime_type === "video/h261") {
    return toList(["h261"]);
  } else if (mime_type === "video/h263") {
    return toList(["h263"]);
  } else if (mime_type === "video/h264") {
    return toList(["h264"]);
  } else if (mime_type === "video/jpeg") {
    return toList(["jpgv"]);
  } else if (mime_type === "video/jpm") {
    return toList(["jpm", "jpgm"]);
  } else if (mime_type === "video/mj2") {
    return toList(["mj2", "mjp2"]);
  } else if (mime_type === "video/mp2t") {
    return toList(["ts", "m2t", "m2ts", "mts"]);
  } else if (mime_type === "video/mp4") {
    return toList(["mp4", "mp4v", "mpg4"]);
  } else if (mime_type === "video/mpeg") {
    return toList(["mpeg", "mpg", "mpe", "m1v", "m2v"]);
  } else if (mime_type === "video/ogg") {
    return toList(["ogv"]);
  } else if (mime_type === "video/quicktime") {
    return toList(["qt", "mov"]);
  } else if (mime_type === "video/vnd.dece.hd") {
    return toList(["uvh", "uvvh"]);
  } else if (mime_type === "video/vnd.dece.mobile") {
    return toList(["uvm", "uvvm"]);
  } else if (mime_type === "video/vnd.dece.pd") {
    return toList(["uvp", "uvvp"]);
  } else if (mime_type === "video/vnd.dece.sd") {
    return toList(["uvs", "uvvs"]);
  } else if (mime_type === "video/vnd.dece.video") {
    return toList(["uvv", "uvvv"]);
  } else if (mime_type === "video/vnd.dvb.file") {
    return toList(["dvb"]);
  } else if (mime_type === "video/vnd.fvt") {
    return toList(["fvt"]);
  } else if (mime_type === "video/vnd.mpegurl") {
    return toList(["mxu", "m4u"]);
  } else if (mime_type === "video/vnd.ms-playready.media.pyv") {
    return toList(["pyv"]);
  } else if (mime_type === "video/vnd.uvvu.mp4") {
    return toList(["uvu", "uvvu"]);
  } else if (mime_type === "video/vnd.vivo") {
    return toList(["viv"]);
  } else if (mime_type === "video/webm") {
    return toList(["webm"]);
  } else if (mime_type === "video/x-f4v") {
    return toList(["f4v"]);
  } else if (mime_type === "video/x-fli") {
    return toList(["fli"]);
  } else if (mime_type === "video/x-flv") {
    return toList(["flv"]);
  } else if (mime_type === "video/x-m4v") {
    return toList(["m4v"]);
  } else if (mime_type === "video/x-matroska") {
    return toList(["mkv", "mk3d", "mks"]);
  } else if (mime_type === "video/x-mng") {
    return toList(["mng"]);
  } else if (mime_type === "video/x-ms-asf") {
    return toList(["asf", "asx"]);
  } else if (mime_type === "video/x-ms-vob") {
    return toList(["vob"]);
  } else if (mime_type === "video/x-ms-wm") {
    return toList(["wm"]);
  } else if (mime_type === "video/x-ms-wmv") {
    return toList(["wmv"]);
  } else if (mime_type === "video/x-ms-wmx") {
    return toList(["wmx"]);
  } else if (mime_type === "video/x-ms-wvx") {
    return toList(["wvx"]);
  } else if (mime_type === "video/x-msvideo") {
    return toList(["avi"]);
  } else if (mime_type === "video/x-sgi-movie") {
    return toList(["movie"]);
  } else if (mime_type === "video/x-smv") {
    return toList(["smv"]);
  } else if (mime_type === "x-conference/x-cooltalk") {
    return toList(["ice"]);
  } else {
    return toList([]);
  }
}
