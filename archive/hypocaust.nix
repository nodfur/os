{ pkgs, lib, config, ... }:

let
  cfg = config.services.hypocaust;
in {
  options.services.hypocaust = {
    enable = lib.mkEnableOption "Hypocaust service";

    domainName = lib.mkOption {
      type = lib.types.str;
      example = "hack.example.com";
    };

    user = lib.mkOption {
      type = lib.types.str;
      example = "mbrock";
    };

    telegram.botName = lib.mkOption {
      type = lib.types.str;
      example = "mbrock_bot";
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      elixir erlang elixir_ls erlang-ls inotify-tools
    ];

    services.postgresql = {
      enable = true;
      enableTCPIP = true;
      authentication = pkgs.lib.mkOverride 10 ''
        local all all trust
        host all all ::1/128 trust
      '';

      ensureDatabases = [
        "hypocaust"
        "nodfur_dev"
      ];

      ensureUsers = [
        {
          name = cfg.user;
          ensurePermissions = {
            "DATABASE hypocaust" = "ALL PRIVILEGES";
            "DATABASE nodfur_dev" = "ALL PRIVILEGES";
          };
        }
      ];
    };

    programs.bash.promptInit = ''
      export CHROME_PROGRAM=${pkgs.google-chrome-beta}/bin/google-chrome-beta

      export HYPOCAUST_DOMAIN_NAME=${cfg.domainName}
      export TELEGRAM_BOT_NAME=${cfg.telegram.botName}
    '';

    services.nginx.virtualHosts = {
      "${cfg.domainName}" = {
        serverName = cfg.domainName;
        locations."/" = {
          proxyPass = "http://127.0.0.1:4000/";
          proxyWebsockets = true;
        };
      };
    };
  };
}
