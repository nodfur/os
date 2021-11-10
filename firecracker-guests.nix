{ self }:
{ config, pkgs, lib, ... }:

let
  rig-start = import ./rig-start.nix {
    inherit pkgs lib;
    base = self.firecracker-rootfs-qemu;
    vmlinux = self.firecracker-vmlinux;
  };

  rig-deploy = import ./rig-deploy.nix {
    inherit pkgs lib;
  };

  firecrackerService = { number, hostname, ... }:
    {
      wantedBy = ["multi-user.target"];
      after = ["network.target"];
      description = "VM ${toString number} (${hostname})";
      serviceConfig = {
        Type = "simple";
        User = "firecracker";
        WorkingDirectory = "/var/lib/rigs";
        ExecStart = "${rig-start}/bin/rig-start ${toString number}";
      };
    };

  instances =
    config.restless.firecracker.instances;
    # lib.forEach
    #   (lib.range 1 config.restless.firecracker.networkSize)
    #   (i: {
    #     number = i;
    #     tapName = "tap${toString i}";
    #     hostname = "vm${toString i}";
    #     localHostname = "vm${toString i}.local";
    #     ip = "172.16.${toString i}.2";
    #   });

in {
  options.restless.firecracker.instances =
    lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [];
    };

  # options.restless.firecracker.networkSize =
  #   lib.mkOption {
  #     type = lib.types.ints.between 0 254;
  #     default = 0;
  #   };

  options.restless.firecracker.hostnameFunction =
    lib.mkOption {
      type = lib.types.functionTo lib.types.string;
      default = { hostname, ... }: hostname;
    };

  config = {
    users.users.firecracker = {
      isSystemUser = true;
      group = "firecracker";
    };

    users.groups.firecracker = {};

    system.activationScripts.makeRigRoot = ''
      mkdir -p /var/lib/rigs
      chown -R firecracker /var/lib/rigs
    '';

    systemd.services = builtins.listToAttrs (
      builtins.map ({ hostname, ... }@x: {
        name = "vm-${hostname}";
        value = firecrackerService x;
      }) instances
    );

    environment.systemPackages = [
      rig-start
      rig-deploy
    ];

    networking.interfaces = builtins.listToAttrs (
      builtins.map (instance:
        let i = toString instance.number;
        in {
          name = instance.tapName;
          value = {
            virtual = true;
            virtualOwner = "firecracker";
            ipv4.addresses = [{
              address = "172.16.${toString i}.1";
              prefixLength = 24;
            }];
          };
        }
      ) instances
    );

    networking.nat.enable = true;

    networking.nat.internalInterfaces =
      builtins.map (x: "tap${toString x.number}") instances;

    networking.extraHosts = lib.concatMapStrings (x: ''
      ${x.ip} ${x.localHostname} vm${toString x.number}.local
    '') instances;

    services.nginx = {
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
    };

    services.nginx.virtualHosts =
      builtins.listToAttrs (
        builtins.map (instance: {
          name = "${instance.hostname}.node.town";
          value = {
            serverAliases = [
              "${instance.hostname}.node.town"
              "${instance.hostname}.tty.node.town"
              "${instance.hostname}.beam.node.town"
            ];
            forceSSL = true;
            useACMEHost = "node.town";
            locations = {
              "/" = {
                proxyPass = "http://${instance.ip}:80";
                proxyWebsockets = true;
              };
            };
          };
        }) instances
      );

    services.dhcpd4 = {
      enable =
        true;
      interfaces =
        lib.forEach instances ({ tapName, ... }: tapName);
      extraConfig = let
        instanceConfig = instance:
          let
            i = toString instance.number;
            ii = lib.fixedWidthString 2 "0" (lib.toHexString instance.number);
          in ''
            subnet 172.16.${i}.0 netmask 255.255.255.0 {
              range 172.16.${i}.2 172.16.${i}.254;
              option routers 172.16.${i}.1;
            }

            host guest-${i} {
              hardware ethernet aa:fc:00:00:00:${ii};
              option host-name "${
                config.restless.firecracker.hostnameFunction instance
              }";
            }
          '';

        in ''
          option domain-name-servers 1.1.1.1, 8.8.8.8;
          option subnet-mask 255.255.0.0;

          ${lib.concatMapStrings instanceConfig instances}
        '';
    };
  };
}
