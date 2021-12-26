{ config, pkgs, ... }:

{
  imports = [
    ./dvorak.nix
    ./password.nix
    ./users/mbrock
    ./users/dbrock
    ./riga.nix
    ./desktop-system.nix
  ];

  hardware.bluetooth.enable = true;

  services.xserver.enable = false;

  os.username = "dbrock";

  networking.hostName = "urbion";

  systemd.services.epap = {
    enable = true;
    description = "Paper Lisp";
    after = ["network.target"];
    serviceConfig = let script = pkgs.writeTextFile {
      name = "epap-start";
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash -li
        set -ex
        cd /os
        nix develop --command \
          sudo -D epap -E sbcl --load boot.lisp \
          --eval '(setq epap::*dry-run* nil)' \
          --eval '(foobar)' \
          --eval '(epap::lets-roll)'
      '';
    }; in {
      User = "dbrock";
      Type = "forking";
      ExecStart = "${pkgs.screen}/bin/screen -dmS lisp ${script}";
      ExecStop = "${pkgs.screen}/bin/screen -S lisp -X quit";
    };
  };
}
