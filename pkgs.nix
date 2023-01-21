{ pkgs, ... }:

{
  nix = {
    settings = {
      substituters = [
        "https://devenv.cachix.org"
      ];
      trusted-public-keys = [
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    binutils
    cachix
    deno
    devenv
    direnv
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
    # zigpkgs.master.latest
    # zls
  ];
}
