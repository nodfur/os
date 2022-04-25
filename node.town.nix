{ pkgs, ... }:

{
  imports = [
    ./beam.nix
    ./git.nix
  ];

  networking.firewall.allowedTCPPorts = [
    80 443
    5900 5901
    8080
  ];

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "mikael@brockman.se";
  security.acme.certs."node.town" = {
    group = "nginx";
    credentialsFile = "/secrets/acme.env";
    dnsProvider = "dnsimple";
    domain = "node.town";
    extraDomainNames = [
      "*.node.town"
      "*.tty.node.town"
      "*.beam.node.town"
    ];
  };

  security.acme.certs."wisp.town" = {
    group = "nginx";
    credentialsFile = "/secrets/acme.env";
    dnsProvider = "dnsimple";
    domain = "wisp.town";
    extraDomainNames = [
      "*.wisp.town"
    ];
  };

  services.nginx = {
    enable = true;

    package = (pkgs.nginxStable.override {
      modules = with pkgs.nginxModules; [
        brotli
      ];
    });

    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = false;

    appendHttpConfig = let
      compressedTypes = ''
        text/plain
        text/html
        text/css
        application/javascript
        application/wasm
        application/json
      '';
    in ''
        gzip on;
        gzip_proxied any;
        gzip_comp_level 6;
        gzip_vary on;
        gzip_types
          ${compressedTypes};
        # Enable brotli compression
        brotli on;
        brotli_comp_level 6;
        brotli_types
          ${compressedTypes};
    '';

    virtualHosts = {
      "node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          root = "/os/www/node.town/";
        };

        locations."/vpn/b14" = {
          proxyPass = "http://127.0.0.1:8080/";
          proxyWebsockets = true;
        };
      };

      "wisp.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          root = "/restless/www/wisp";
        };
      };

      "photos.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          root = "/restless/www/2021-08-30";
        };
      };

      "wisp.town" = {
        forceSSL = true;
        useACMEHost = "wisp.town";
        locations."/" = {
          root = "/src/wisp/web";
        };
      };

      "zig.wisp.town" = {
        forceSSL = true;
        useACMEHost = "wisp.town";
        root = "/src/zigjs";
      };

      "git.wisp.town" = {
        forceSSL = true;
        useACMEHost = "wisp.town";
        root = "/srv/git/wisp";
        locations."~ (/.*)".extraConfig = ''
          fastcgi_pass unix:/run/fcgiwrap.sock;
          fastcgi_param SCRIPT_FILENAME     ${pkgs.git}/libexec/git-core/git-http-backend;
          fastcgi_param GIT_HTTP_EXPORT_ALL "";
          fastcgi_param GIT_PROJECT_ROOT    /srv/git/wisp;
          fastcgi_param PATH_INFO           $1;
          include ${pkgs.nginx}/conf/fastcgi_params;
          include ${pkgs.nginx}/conf/fastcgi.conf;
        '';
      };

      "api.wisp.town" = {
        forceSSL = true;
        useACMEHost = "wisp.town";
        locations."/" = {
          proxyPass = "http://127.0.0.1:8000";
          extraConfig = ''
            chunked_transfer_encoding off;
            proxy_buffering off;
            proxy_cache off;
            proxy_redirect default;
          '';
        };
      };

      "cors.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          proxyPass = "http://127.0.0.1:9999";
          extraConfig = ''
            chunked_transfer_encoding off;
            proxy_buffering off;
            proxy_cache off;
            proxy_redirect default;
          '';
        };
      };

      "wisp-dev.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          root = "/src/wisp/web/dist";
        };
      };

      "b14.wisp.town" = {
        forceSSL = true;
        useACMEHost = "wisp.town";
        locations."/" = {
          root = "/src/wisp/web";
        };
      };

      "urbit.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          proxyPass = "http://127.0.0.1:8080";
          extraConfig = ''
            chunked_transfer_encoding off;
            proxy_buffering off;
            proxy_cache off;
            proxy_redirect default;
          '';
        };
      };

      "root.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          root = "/restless/www/root";
        };
      };

      "b14.beam.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
      };
    };
  };

  services.fcgiwrap = {
    enable = true;
    user = "git";
    group = "wheel";
  };
}
