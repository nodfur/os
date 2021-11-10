{ pkgs, ... }:

{
  systemd.services.epmd = {
    wantedBy = ["multi-user.target"];
    after = ["network.target"];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.erlang}/bin/epmd
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    cmake
    binutils
  ];
}
