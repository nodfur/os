{ config, pkgs, ... }:

{
  services.tailscale.enable = true;

  nixpkgs.config.allowUnfree = true;
  
  environment.systemPackages = with pkgs; [
    nodejs-16_x
    tdesktop
    unzip
    wget
    python3
    ruby
  ];

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  nixpkgs.config.permittedInsecurePackages = [
    "libgit2-0.27.10"
  ];

  console.font = "Lat2-Terminus16";
}
