{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    gdb
    clang
    clang-tools
    emscripten
    wasmtime
  ];

  system.activationScripts = {
    setupCommonLisp = let user = config.os.username; in ''
      mkdir -p /home/${user}/common-lisp
      cd /home/${user}/common-lisp
      ln -sf /os/epap
      chown ${user}:users . epap
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
        cd /os
        nix develop --command \
          ${pkgs.bash}/bin/bash -ic "cd epap && sbcl --load boot.lisp \
          --eval '(setq epap::*dry-run* t)' \
          --eval '(foobar)'"
      '';
    }; in {
      User = "mbrock";
      Type = "forking";
      ExecStart = "${pkgs.screen}/bin/screen -dmS lisp ${script}";
      ExecStop = "${pkgs.screen}/bin/screen -S lisp -X quit";
    };
  };
}
