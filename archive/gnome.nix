{ pkgs, config, ... }: {
  services.xserver =
    if config.os.vm then {} else {
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      displayManager.gdm.wayland = true;
    };

}
