{ settings
, stdenv
, imagemagick
, material-design-icons
, gixy
, luajit
, envsubst
, openresty
, writeText
, writeTextDir
, writeTextFile
, checkedShellScript
}:

let
  nginxConf =
    writeNginxConf "nginx.conf"
      ''
        daemon off;
        error_log stderr info;

        events {}

        http {
            include ${openresty}/nginx/conf/mime.types;
            default_type application/ocet-stream;

            gzip off;
            sendfile on;

            keepalive_timeout 65s;

            server {
                listen ${settings.port};

                # add_header Content-Security-Policy "default-src 'self'; style-src 'unsafe-inline'";
                # add_header X-Content-Type-Options "nosniff";
                add_header X-Frame-Options "SAMEORIGIN";
                add_header X-XSS-Protection "1; mode=block";

                root ${settings.webappWebroot};

                location = /favicon.ico {
                    alias ${favicon};
                }

                location /api/ {
                    more_clear_input_headers Accept-Encoding;
                    access_by_lua_file ${antiCsrf};
                    proxy_pass ${settings.apiURI};
                }

                location /app/ {
                    alias ${settings.webappDir}/;
                    try_files $$uri /;
                }

                location /docs/ {
                    alias ${settings.docsDir}/;
                }

                location /fonts/material-design-icons/ {
                    alias ${material-design-icons}/share/fonts/;
                }

                location = /healthcheck {
                    content_by_lua_file ${healthcheck};
                }
            }
        }
      '';

  favicon =
    stdenv.mkDerivation rec {
      name = "favicon.ico";
      src = ../webapp/assets/favicon.png;
      phases = [ "buildPhase" ];
      buildPhase =
        ''
          ${imagemagick}/bin/convert -flatten -background none -resize 16x16 ${src} $out
        '';
    };

  antiCsrf =
    writeLuaScript "anticsrf.lua"
      ''
        -- TODO: enable CSRF protection for production

        -- First line of defense: Check that origin or referer is set and that they
        -- match the current host (using ngx.var.http_origin and ngx.var.http_referer)

        -- Defense in depth: Require a custom header for API requests, which can
        -- only be set by requests from the same origin

        -- if ngx.req.get_headers()['X-Requested-By'] == nil then
        --     ngx.header.content_type = 'text/plain'
        --     ngx.say('Missing X-Requested-By header - not allowed to mitigate CSRF')
        --     ngx.exit(405)
        -- end
      '';

  rewriteOpenApi =
    writeLuaScript "openapi.lua"
      ''
        local cjson = require "cjson"
        cjson.decode_array_with_array_mt(true)

        local res = ngx.location.capture("/api/")
        api = cjson.decode(res.body)

        api["basePath"] = "/api/"
        api["host"] = ""

        ngx.say(cjson.encode(api))
      '';

  healthcheck =
    writeLuaScript "openapi.lua"
      ''
        local res = ngx.location.capture("/app.js")

        if res.status ~= 200 then
           ngx.status = 503
           return
        end

        local res = ngx.location.capture("/api/")

        if res.status ~= 200 then
           ngx.status = 503
           return
        end
      '';

  writeNginxConf =
    name: text:
      writeTextFile {
        inherit name text;
        checkPhase =
          ''
            ${gixy}/bin/gixy $out
          '';
      };

  writeLuaScript =
    name: text:
      writeTextFile {
        inherit name text;
        checkPhase =
          ''
            ${luajit}/bin/luajit -bl $out > /dev/null
          '';
        };

  binPrefix =
    "${settings.binPrefix}ingress-";
in
{ run =
    checkedShellScript "${binPrefix}run"
      ''
        mkdir -p "${settings.ingressDir}"/{logs,conf}
        touch "${settings.ingressDir}"/logs/{error.log,access.log}
        ${envsubst}/bin/envsubst -i ${nginxConf} \
          -o "${settings.ingressDir}/conf/nginx.conf"

        exec ${openresty}/bin/openresty -p "${settings.ingressDir}"
      '';
}
