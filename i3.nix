{ config, pkgs, ... }:

{
  imports = [./picom.nix];

  environment.systemPackages = with pkgs; [
    dmenu
    i3status
    i3lock
    lxappearance
    unclutter-xfixes
    sxhkd
  ];

  environment.pathsToLink = ["/libexec"];

  programs.dconf.enable = true;

  services.xserver = {
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "none+i3";
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };

  environment.etc.sxhkdrc.text = ''
    XF86MonBrightnessUp
      ${pkgs.xorg.xbacklight}/bin/xbacklight +20
    XF86MonBrightnessDown
      ${pkgs.xorg.xbacklight}/bin/xbacklight =1
    XF86AudioMute
      ${pkgs.alsaUtils}/bin/amixer set Master mute
    shift + XF86AudioMute
      ${pkgs.alsaUtils}/bin/amixer set Master unmute
    XF86AudioLowerVolume
      ${pkgs.alsaUtils}/bin/amixer set Master 10%-
    XF86AudioRaiseVolume
      ${pkgs.alsaUtils}/bin/amixer set Master 10%+
    shift + XF86AudioLowerVolume
      ${pkgs.alsaUtils}/bin/amixer set Master 25%-
    shift + XF86AudioRaiseVolume
      ${pkgs.alsaUtils}/bin/amixer set Master 25%+
  '';

}
