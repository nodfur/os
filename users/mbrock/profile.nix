{ pkgs, config, ... }:

{
  imports = [
    ../../user-emacs.nix
    ../../user-mail.nix
    ../../user-basic.nix
  ];

  programs.git = {
    enable = true;
    userName = "Mikael Brockman";
    userEmail = "mikael@brockman.se";
    signing = {
      key = "7C64800F5400D9BA";
      signByDefault = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
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

  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
    };
    defaultApplications = {
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
    };
  };
}
