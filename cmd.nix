{ config, pkgs, lib, ... }: let
  monoFont = config.os.monospace.family;
  monoSize = config.os.monospace.size;
  monoSizeSmall = monoSize / 2;

  using = pkgs: cmd: ''
    export PATH=${lib.makeBinPath pkgs}:$PATH
    ${cmd}
  '';

  commands = {
    os-terminal = using [pkgs.xterm] ''
      xterm -r -s -b 18 -w 0 \
        -fs ${toString monoSize} -fa '${monoFont}' "$@"
    '';

    os-miniterm = using [pkgs.xterm] ''
       xterm -r -s -b 18 -w 0 \
         -fs ${toString monoSizeSmall} -fa '${config.os.monospace.family}' "$@"
    '';

    os-xrandr-rotate-all = using [pkgs.xorg.xrandr] ''
      xrandr | grep ' connected' | awk '{print $1}' |
      while read o; do
        xrandr --output "$o" --rotate "$1"
      done
    '';

    os-i3-gaps = using [pkgs.i3-gaps] ''
      i3-msg gaps horizontal current set $1
      i3-msg gaps vertical current set $2
    '';
  };

in {
  environment.systemPackages =
    lib.mapAttrsToList pkgs.writeShellScriptBin commands;
}
