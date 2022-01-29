{ lib, ... }: {
  options = {
    os.graphical = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    os.gl = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    os.vnc.size.height = lib.mkOption {
      type = lib.types.int;
    };

    os.vnc.size.width = lib.mkOption {
      type = lib.types.int;
    };

    os.username = lib.mkOption {
      type = lib.types.str;
    };

    os.monospace.family = lib.mkOption {
      type = lib.types.str;
      default = "DM Mono";
    };

    os.monospace.size = lib.mkOption {
      type = lib.types.int;
      default = 16;
    };

    os.vm = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    os.wayland = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    os.gmail = {
      enable =
        lib.mkEnableOption "Gmail";

      name =
        lib.mkOption { type = lib.types.str; };

      address =
        lib.mkOption { type = lib.types.str; };

      passwordCommand =
        lib.mkOption {
          type = lib.types.str;
          default = "pass smtp.googlemail.com";
        };

      patterns =
        lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
        };
    };
  };
}
