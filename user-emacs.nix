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
      exwm
      humanoid-themes
      lsp-mode
      lsp-ui
      lua-mode
      magit
      nix-mode
      nov
      paredit
      pdf-tools
      projectile
      rainbow-delimiters
      selectrum
      selectrum-prescient
      treemacs
      treemacs-magit
      treemacs-projectile
      which-key
      whitespace-cleanup-mode
      zenburn-theme
      vterm

      pkgs.nano-emacs
      pkgs.urbit-emacs
    ];
  };
}
