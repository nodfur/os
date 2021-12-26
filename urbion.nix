{ config, pkgs, ... }:

{
  imports = [
    ./dvorak.nix
    ./password.nix
#    ./x.nix
#    ./i3.nix
    ./users/mbrock
    ./users/dbrock
    ./riga.nix
    ./desktop-system.nix
  ];

  services.xserver.enable = false;

  os.username = "dbrock";
  # os.monospace.size = 18;

  # services.xserver.resolutions = [
  #   { x = 1920; y = 1280; }
  #   { x = 1920; y = 1200; }
  # ];
  #
  # services.xserver.xrandrHeads = [{
  #   output = "HDMI-1";
  #   primary = true;
  #   monitorConfig = ''
  #     Option "Rotate" "left"
  #   '';
  # }];

  networking.hostName = "urbion";

  environment.systemPackages = with pkgs; [
    # firefox
  ];

  system.activationScripts = {
    epapBoot = ''
      cd /home/mbrock/common-lisp/epap
      ./epap-boot
    '';
  };

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
        cd ~/common-lisp/epap
        nix develop --command \
          sudo -E sbcl --core epap-core \
          --eval '(setq epap::*dry-run* nil)' \
          --eval '(foobar)' \
          --eval '(epap::lets-roll)'
      '';
    }; in {
      User = "mbrock";
      Type = "forking";
      ExecStart = "${pkgs.screen}/bin/screen -dmS lisp ${script}";
      ExecStop = "${pkgs.screen}/bin/screen -S lisp -X quit";
    };
  };
}
