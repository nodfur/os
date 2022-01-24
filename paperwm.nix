{ pkgs, ... }:
{
  os.wayland = true;
  services.xserver.displayManager.gdm.wayland = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  environment.systemPackages = with pkgs; [
    gnomeExtensions.appindicator
    gnomeExtensions.paperwm
    gnome3.gnome-tweak-tool
  ];

  services.dbus.packages = with pkgs; [
    gnome3.dconf
  ];

  services.udev.packages = with pkgs; [
    gnome3.gnome-settings-daemon
  ];

  hardware.pulseaudio.enable = false;
}
