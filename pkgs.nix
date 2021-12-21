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
    # fzf
    htop
    nix-prefetch-git
    ripgrep
    tmux
    # sqlite

    cachix

    zig

    entr
    # netpbm

    sbcl
    # lispPackages.cffi
    # lispPackages.alexandria
    # lispPackages.trivial-features
    # lispPackages.babel

    restless-emacs

    # guile_3_0
  ];
}
