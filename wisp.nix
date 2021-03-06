{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    clang
    clang-tools
    ddd
    emscripten
    esbuild
    gdb
    jq
    nodePackages.typescript-language-server
    wasmtime
    zigpkgs.master.latest
    zls
  ];

  system.activationScripts = {
    setupCommonLisp = let user = config.os.username; in ''
      mkdir -p /home/${user}/common-lisp
      cd /home/${user}/common-lisp
      for x in epap wisp; do
        if [ ! -e $x ]; then
          echo linking $PWD/$x -> /os/$x
          ln -s /os/"$x"
        fi
      done
      chown ${user}:users . *
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
