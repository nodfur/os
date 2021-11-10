{ pkgs, ... }:

{
  imports = [
    ./dvorak.nix
    ./password.nix
    ./x.nix
    ./i3.nix
    ./users/mbrock
    ./riga.nix
    ./desktop-system.nix
  ];

#  hardware.video.hidpi.enable = true;
#  services.xserver.dpi = 180;

  services.xserver.resolutions = [
    { x = 1920; y = 1280; }
  ];

  networking.hostName = "urbion";

  environment.systemPackages = with pkgs; [
    firefox
  ];
}
