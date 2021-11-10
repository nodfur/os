{ config, ... }:

{
  services.picom = {
    enable = true;
    shadow = true;
    vSync = true;

    backend =
      if config.os.gl
      then "glx"
      else "xrender";

    settings = {
      "corner-radius" = 14;
      "shadow-radius" = 24;
      "rounded-corners-exclude" = "window_type = 'dock'";
    };
  };
}
