{
  programs.git = {
    enable = true;
  };

  imports = [
    ./user-emacs.nix
    ./user-mail.nix
    ./user-basic.nix
  ];
}
