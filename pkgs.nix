{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    binutils
    file
    gcc
    git
    gnumake
    pciutils
    pstree

    ag
    cmake
    fzf
    htop
    nix-prefetch-git
    ripgrep
    tmux
    sqlite

    cachix
  ];
}
