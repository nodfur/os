{ config, pkgs, ... }:

{
  imports = [
    ./cmd.nix
  ];
  
  services.tailscale.enable = true;

  nixpkgs.config.allowUnfree = true;
  
  environment.systemPackages = with pkgs; [
    ffmpeg
    scrot
    signal-desktop
    tdesktop
    yarn2nix
  ];

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  console.font = "Lat2-Terminus16";
  
  system.activationScripts.fixAppleKeyboard = ''
    file=/sys/module/hid_apple/parameters/swap_opt_cmd
    test -f $file && echo 1 > $file
  '';
}
