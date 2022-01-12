{ config, pkgs, ... }:

{
  imports = [./picom.nix];

  environment.systemPackages = with pkgs; [
    dmenu
    i3status
    i3lock
    lxappearance
    unclutter-xfixes
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
}
