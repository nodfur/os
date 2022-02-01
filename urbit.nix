{ pkgs, lib, config, ... }:

lib.mkIf config.os.urbit.enable {
  systemd.services.urbit = {
    enable = true;
    description = "Urbit";
    after = ["network.target"];
    serviceConfig = let script = pkgs.writeTextFile {
      name = "urbit-start";
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash -li
        set -ex
        cd ~/urbit
        urbit ${config.os.urbit.id}
      '';
    }; in {
      User = config.os.username;
      Type = "forking";
      ExecStart = "${pkgs.screen}/bin/screen -dmS urbit ${script}";
      ExecStop = "${pkgs.screen}/bin/screen -S urbit -X quit";
    };
  };
}
