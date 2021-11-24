{ pkgs, config, ... }:

{
  imports = [
    ../../user-emacs.nix
    ../../user-mail.nix
  ];

  programs.git = {
    enable = true;
    userName = "Mikael Brockman";
    userEmail = "mikael@brockman.se";
  };

  programs.password-store = {
    enable = true;
  };

  programs.vscode = {
    # enable = true;
    extensions = with pkgs.vscode-extensions; [
      github.copilot
      eamodio.gitlens
    ];
  };
}
