{ pkgs, config, ... }: {
  imports = [
    ./desktop.nix
    ./dvorak.nix
    ./i3.nix
    ./users/mbrock
    ./wisp.nix
  ] ++ if config.os.graphical then [
    ./i3.nix
    ./google-fonts.nix
    ./other-fonts.nix
  ];

  os.gmail = {
    address = "mikael@brockman.se";
    name = "Mikael Brockman";
    patterns = [
      "INBOX" "[Gmail]/Sent Mail" "[Gmail]/All Mail"
    ];
  };
}
