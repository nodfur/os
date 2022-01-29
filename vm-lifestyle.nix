{
  nixpkgs,
  domain,
  number,
  username,
  sshKeys,
  keys,
  ports
}:

{ pkgs, lib, config, ... }:

let
  git = "${pkgs.git}/bin/git";

  mmdsAddress = "169.254.169.254";

in {
  options.firecracker = {
    guestIndex = lib.mkOption {
      type = lib.types.int;
    };
  };

  imports = [
    (import ./firecracker-base-system.nix {
      inherit nixpkgs;
      isContainer = true;
    })

    ./firecracker-networking.nix

    ./password.nix
    ./pkgs.nix
    ./vnc.nix

    ./users/mbrock
  ];

  config = {
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "google-chrome-beta"
    ];

    firecracker.guestIndex = number;
    os.username = username;
    os.vm = true;
    os.vnc.size.width = 1920;
    os.vnc.size.height = 1080;

    home-manager.users."${config.os.username}" =
      import ./user.nix;

    networking.interfaces.eth0.ipv4.routes = [
      {
        address = mmdsAddress;
        prefixLength = 32;
      }
    ];

    networking.hosts."${mmdsAddress}" = ["mmds.local"];

    environment.systemPackages = with pkgs; [
      (import ./rig-setup-user.nix { inherit pkgs; })
      (pkgs.writeShellScriptBin "sudo-user" ''
        sudo -iu "${config.os.username}" "$@"
      '')
      (pkgs.writeShellScriptBin "os-tailscale" ''
        set -e
        sudo tailscale up
        ( set -x; tailscale status )

        echo
        echo "Your node is connected to your Tailscale VPN network."
        echo "If you have Tailscale Magic DNS set up, you can run $(tput bold)ssh ${config.os.username}$(tput sgr0)."
        echo
        echo Internet web terminal URL: https://${config.os.username}.tty.node.town
        echo "  Username: $(tput bold)${config.os.username}$(tput sgr0)"
        echo "  Password: $(tput bold)$(cat /password)$(tput sgr0)"
        echo
      '')
    ];

   users.users = {
     "${config.os.username}" = {
       uid = 1000;
       isNormalUser = true;
       initialHashedPassword = "";
       extraGroups = ["wheel"];
       openssh.authorizedKeys.keys = [
         "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFOnlJXoU5gKCV1s69ROeA5R4P7uZDYvIrAO6nzVqgIS admin@example.org"
       ] ++ sshKeys;
     };
   };

   nix.trustedUsers = [config.os.username];

   networking.hostName = lib.mkOverride 10 config.os.username;

#   systemd.services.ttyd = {
#      description = "ttyd Web Server Daemon";
#
#      wantedBy = [ "multi-user.target" ];
#
#      serviceConfig = {
#        User = "root";
#      };
#
#      script = let
#      in ''
#        PASSWORD=$(cat /password)
#        ${pkgs.ttyd}/bin/ttyd \
#          --credential ${config.os.username}:"$PASSWORD" \
#          --port ${toString ports.ttyd} \
#          ${pkgs.shadow}/bin/login -f ${config.os.username}
#      '';
#    };

    services.ttyd = {
      enable = true;
      port = ports.ttyd;
      username = config.os.username;
      passwordFile = "/password";
      clientOptions = {
        fontSize = "16";
      };
    };

    networking.firewall.allowedTCPPorts = [80];

    services.nginx = {
      enable = true;
      virtualHosts = {
        tty = {
          serverName = "*.tty.${domain}";
          locations = {
            "/" = {
              proxyPass = "http://127.0.0.1:${toString ports.ttyd}/";
              proxyWebsockets = true;
            };
          };
        };

        dev = {
          serverName = "dev.*";
          root = (pkgs.linkFarmFromDrvs "webroot" [
            (pkgs.writeTextFile {
              name = "index.html";
              text = ''
                <!doctype html>
                <b style="margin: 8em auto;">Hello.</b>
              '';
            })
          ]);
        };
      };
    };
  };
}
