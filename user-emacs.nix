{ pkgs, ... }:

{
  home.file = {
    ".emacs.d/init.el".source = ./emacs-init.el;
  };

  home.packages = [
    (pkgs.makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeType = "x-scheme-handler/org-protocol";
    })
  ];
}
