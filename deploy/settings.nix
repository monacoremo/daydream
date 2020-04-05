{ lib
, appName
}:
let
  prefix = "${lib.toUpper appName}_";
  dbPrefix = "${prefix}DB_";
  apiPrefix = "${prefix}API_";
  webappPrefix = "${prefix}WEBAPP_";
  ingressPrefix = "${prefix}INGRESS_";
  docsPrefix = "${prefix}DOCS_";

  vars = {
    sourceDir = "${prefix}SRC";
    port = "${prefix}PORT";
    dir = "${prefix}DIR";
    URI = "${prefix}URI";
    dbURI = "${dbPrefix}URI";
    dbDir = "${dbPrefix}DIR";
    dbLogfile = "${dbPrefix}LOGFILE";
    dbSrc = "${dbPrefix}SRC";
    dbHost = "${dbPrefix}HOST";
    dbName = "${dbPrefix}NAME";
    dbSuperuser = "${dbPrefix}SUPERUSER";
    dbSuperuserPassword = "${dbPrefix}SUPERUSER_PW";
    dbApiserverPassword = "${dbPrefix}APISERVER_PW";
    dbSetupHost = "${dbPrefix}SETUP_HOST";
    dbSetupURI = "${dbPrefix}SETUP_URI";
    dbSuperuserURI = "${dbPrefix}SUPERUSER_URI";
    dbApiserverURI = "${dbPrefix}APISERVER_URI";
    apiLogfile = "${apiPrefix}LOGFILE";
    apiDir = "${apiPrefix}DIR";
    apiSocket = "${apiPrefix}SOCKET";
    apiConfig = "${apiPrefix}CONFIG";
    apiURI = "${apiPrefix}URI";
    webappLogfile = "${webappPrefix}LOGFILE";
    webappSrc = "${webappPrefix}SRC";
    webappDir = "${webappPrefix}DIR";
    webappWebroot = "${webappPrefix}WEBROOT";
    ingressLogfile = "${ingressPrefix}LOGFILE";
    ingressDir = "${ingressPrefix}DIR";
    docsSrc = "${docsPrefix}SRC";
    docsDir = "${docsPrefix}DIR";
    docsLogfile = "${docsPrefix}LOGFILE";
  };

  vals = lib.mapAttrs (name: value: "$" + value) vars;

  fixed = {
    inherit appName;

    binPrefix = "${lib.toLower appName}-";
  };
in
fixed // vals // { inherit vars; }
