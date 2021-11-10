{ config, pkgs, ... }:

{
  imports = [./picom.nix];

  environment.systemPackages = [pkgs.awesome];

  services.xserver = {
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "none+awesome";
    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages; [
        luarocks
      ];
    };
  };
}
