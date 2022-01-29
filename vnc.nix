{ pkgs, config, lib, ... }:

{
  imports = [
    ./x.nix
  ];

  config = {
    networking.firewall.allowedTCPPorts = [5900];

    systemd.services.vnc = {
      wantedBy = ["multi-user.target"];
      after = ["network.target"];
      serviceConfig = {
        User = config.os.username;
        Group = "users";
        ExecStart =
          let
            inherit (config.os.vnc.size) width height;
            sizeString = "${toString width}x${toString height}x16";
          in pkgs.writeShellScript "vnc" ''
            #!/usr/bin/env bash
            set -ex
            export DISPLAY=:1
            export PATH=/var/nix/profiles/default/bin:$PATH
            export PATH=/run/current-system/sw/bin:$PATH
            export PATH=/etc/profiles/per-user/$USER/bin:$PATH
            export PATH=/run/wrappers/bin:$PATH

            ${pkgs.xvfb-run}/bin/xvfb-run -n 1 -s "-screen 0 ${sizeString}" \
              ${pkgs.dbus.dbus-launch} --exit-with-session \
                ${config.services.xserver.windowManager.i3.package}/bin/i3 &

            sleep 3

            ${pkgs.x11vnc}/bin/x11vnc -forever -shared -quiet -display :1 \
              -noxrecord -xkb -passwd $(cat /password) &

            wait
          '';
      };
    };
  };
}
