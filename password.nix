{ pkgs, ... }: {
  config = {
    system.activationScripts.makePassword = ''
      if [ ! -f /password ]; then
        ${pkgs.curl}/bin/curl mmds.local/password > /password
      fi
      echo "Rig password: $(cat /password)"
    '';
  };
}
