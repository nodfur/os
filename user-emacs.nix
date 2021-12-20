{ pkgs, ... }:

{
  home.file = {
    ".emacs.d/init.el".source = ./emacs-init.el;
  };
}
