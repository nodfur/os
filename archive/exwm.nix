{ pkgs, lib, config, ... }:
{
  services.xserver.displayManager =
    if config.os.vm then {} else {
      lightdm.enable = true;
      lightdm.greeter.enable = false;
      autoLogin.enable = true;
    };

  services.xserver.windowManager.session = lib.singleton {
    name = "exwm";
    start = ''
      ${pkgs.dbus.dbus-launch} --exit-with-session emacs -mm --fullscreen \
        -f exwm-config-restless
    '';
  };
}
