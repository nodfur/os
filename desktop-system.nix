{ config, pkgs, ... }:

{
  services.tailscale.enable = true;

  nixpkgs.config.allowUnfree = true;
  
  environment.systemPackages = with pkgs; [
    nix-tree
    nodejs-16_x
    python3
    rlwrap
    ruby
    scrot
    tdesktop
    unzip
    wget
    texlive.combined.scheme-full
    evince
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
