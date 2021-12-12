{ pkgs, ... }:

{
  home.file = {
    ".emacs.d/init.el".source = ./emacs-init.el;
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      ag
      company
      company-nixos-options
      default-text-scale
      elixir-mode
      lsp-mode
      lsp-ui
      magit
      nix-mode
      paredit
      pdf-tools
      projectile
      rainbow-delimiters
      selectrum
      selectrum-prescient
      vterm
      which-key
      whitespace-cleanup-mode
      zenburn-theme
      zig-mode

      slime

      pkgs.nano-emacs
      pkgs.urbit-emacs
    ];
  };
}
