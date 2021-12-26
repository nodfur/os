{ pkgs, config, ... }:

{
  imports = [
    ../../user-emacs.nix
    ../../user-mail.nix
  ];

  programs.git = {
    enable = true;
    userName = "Daniel Brockman";
    userEmail = "daniel@brockman.se";
  };

  programs.password-store = {
    enable = true;
  };
}
