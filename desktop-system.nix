{ config, pkgs, ... }:

{
  imports = [
    ./cmd.nix
  ];
  
  services.tailscale.enable = true;

  nixpkgs.config.allowUnfree = true;
  
  environment.systemPackages = with pkgs; [
    beancount
    deno
    evince
    ffmpeg
    git-filter-repo
    gnupg
    nix-tree
    nodejs-16_x
    linuxPackages.perf
    perf-tools
    python3
    rlwrap
    ruby
    scrot
    signal-desktop
    tdesktop
    texlive.combined.scheme-full
    unzip
    wabt
    wasmer
    wget
  ];

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  nixpkgs.config.permittedInsecurePackages = [
    "libgit2-0.27.10"
  ];

  console.font = "Lat2-Terminus16";
  
  system.activationScripts.fixAppleKeyboard = ''
    file=/sys/module/hid_apple/parameters/swap_opt_cmd
    test -f $file && echo 1 > $file
  '';
}
