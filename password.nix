{ pkgs, lib, ... }: {
  options.os.username = lib.mkOption {
    type = lib.types.str;
  };

  config = {
    system.activationScripts.makePassword = ''
      if [ ! -f /password ]; then
        ${pkgs.curl}/bin/curl mmds.local/password > /password
      fi
      echo "Rig password: $(cat /password)"
    '';
  };
}
