{ pkgs, lib, config, ... }:

let cfg = config.services.ssbot;

in {
  options.services.ssbot = {
    enable = lib.mkEnableOption "ssbot";
    botName = lib.mkOption { type = lib.types.str; };
    botVhost = lib.mkOption { type = lib.types.str; };
    dbVhost = lib.mkOption { type = lib.types.str; };
  };

  config = {
    services.couchdb = {
      enable = true;
      package = pkgs.couchdb3;
      adminPass = "couchdb";
    };

    services.nginx.virtualHosts."${cfg.dbVhost}" = {
      serverName = cfg.dbVhost;
      forceSSL = true;
      useACMEHost = "node.town";
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.couchdb.port}";
        extraConfig = ''
          limit_except GET {
            allow 127.0.0.1;
            deny all;
          }
        '';
      };
    };
  };
}
