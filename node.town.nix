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
  security.acme.email = "mikael@brockman.se";
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

  services.nginx = {
    enable = true;
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

      "wisp-dev.node.town" = {
        forceSSL = true;
        useACMEHost = "node.town";
        locations."/" = {
          root = "/os/wisp/dist";
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
}
