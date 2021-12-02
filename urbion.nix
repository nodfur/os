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

  services.xserver.enable = false;

  services.udev.extraRules = ''
    KERNEL=="mem", GROUP=="kmem", MODE="0660"
    SUBSYSTEM=="bcm2835-gpiomem", KERNEL=="gpiomem", GROUP="wheel",MODE="0660"
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", ACTION=="add", RUN+="${pkgs.bash}/bin/bash -c 'chown root:wheel  /sys/class/gpio/export /sys/class/gpio/unexport ; chmod 220 /sys/class/gpio/export /sys/class/gpio/unexport'"
    SUBSYSTEM=="gpio", KERNEL=="gpio*", ACTION=="add",RUN+="${pkgs.bash}/bin/bash -c 'chown root:wheel /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value ; chmod 660 /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value'"
  '';

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
