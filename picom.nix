{ config, ... }:

{
  services.picom = {
    enable = true;
    shadow = true;
    vSync = config.os.gl;

    backend =
      if config.os.gl
      then "glx"
      else "xrender";

    settings = {
      "corner-radius" = 10;
      "shadow-radius" = 48;
      "shadow-offset-x" = -8;
      "shadow-offset-y" = -8;
      "shadow-opacity" = 0.4;
      "rounded-corners-exclude" = "window_type = 'dock'";
    };
  };
}
