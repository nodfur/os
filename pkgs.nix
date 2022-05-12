{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    binutils
    cachix
    deno
    entr
    file
    gcc
    gdb
    git
    gnumake
    gnupg
    guile_3_0
    htop
    linuxPackages.perf
    nix-prefetch-git
    nodejs-16_x
    pciutils
    perf-tools
    pstree
    python3
    restless-emacs
    ripgrep
    rlwrap
    ruby
    sbcl
    screen
    silver-searcher
    tmux
    unzip
    wget
    yarn
    zigpkgs.master.latest
    zls
  ];
}
