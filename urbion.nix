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

  os.username = "mbrock";
  os.monospace.size = 18;

  services.xserver.resolutions = [
    { x = 1920; y = 1280; }
    { x = 1920; y = 1200; }
  ];

  services.xserver.xrandrHeads = [{
    output = "HDMI-1";
    primary = true;
    monitorConfig = ''
      Option "Rotate" "left"
    '';
  }];

  networking.hostName = "urbion";

  environment.systemPackages = with pkgs; [
    firefox
  ];
}
