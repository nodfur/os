{ pkgs, config, ... }: {
  imports = [
    ./desktop.nix
    ./dvorak.nix
    ./i3.nix
    ./users/mbrock
    ./wisp.nix
  ] ++ (
    if true then [
      ./i3.nix
      ./google-fonts.nix
      ./other-fonts.nix
    ] else []
  );

  os.graphical = true;

  os.gmail = {
    address = "mikael@brockman.se";
    name = "Mikael Brockman";
    patterns = [
      "INBOX" "[Gmail]/Sent Mail" "[Gmail]/All Mail"
    ];
  };
}
