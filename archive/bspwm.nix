{ pkgs, config, ... }:

{
  imports = [
    ./menu.nix
    ./picom.nix
  ];

  environment.systemPackages = with pkgs; [
    bspwm sxhkd

    (pkgs.writeShellScriptBin "os-terminal" ''
     ${pkgs.xterm}/bin/xterm -r -s -b 4 -fs ${toString config.os.monospace.size} -fa '${config.os.monospace.family}' "$@"
    '')
  ];

  services.xserver.windowManager.bspwm.enable = !config.os.vm;
  services.xserver.windowManager.bspwm.sxhkd.configFile = "/etc/sxhkdrc";

  services.xserver.windowManager.bspwm.configFile =
    pkgs.writeShellScript "bspwmrc" ''
      #!/usr/bin/env bash

      set -ex

      ${pkgs.sxhkd}/bin/sxhkd -m -1 -c /etc/sxhkdrc &

      ${pkgs.unclutter-xfixes}/bin/unclutter &

      ${pkgs.hsetroot}/bin/hsetroot -cover "${./bg1.jpg}"

      ${pkgs.bspwm}/bin/bspc monitor -d I II
      ${pkgs.bspwm}/bin/bspc config border_width 4
      ${pkgs.bspwm}/bin/bspc config window_gap 14
      ${pkgs.bspwm}/bin/bspc config focused_border_color '#aaaa66'
      ${pkgs.bspwm}/bin/bspc config split_ratio 0.6
      ${pkgs.bspwm}/bin/bspc config borderless_monocle true
      ${pkgs.bspwm}/bin/bspc config gapless_monocle true
      ${pkgs.bspwm}/bin/bspc rule -a Emacs state=tiled

      os-terminal &
    '';

  environment.etc.sxhkdrc.text = ''
    F12
      os-terminal

    ctrl + Return
      restless-menu

    Pause
      bspc monitor -f next

    {F1,F2}
      bspc node -f {next,prev}.local.!hidden.window

    shift + Pause
      bspc node -m next --follow

    shift + F1
      bspc node -s next --follow
  '';
}
