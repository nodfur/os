{ lib, ... }: {
  options = {
    os.gl = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };
}
