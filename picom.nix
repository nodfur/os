{ config, ... }:

{
  services.picom = {
    enable = true;
    vSync = config.os.gl;

    backend =
      if config.os.gl
      then "glx"
      else "xrender";

    shadow = true;
    settings = {
      "corner-radius" = 8;
      "shadow-radius" = 60;
      "shadow-offset-x" = -8;
      "shadow-offset-y" = -8;
      "shadow-opacity" = 0.4;
      "rounded-corners-exclude" = "window_type = 'dock'";
    };
  };
}
