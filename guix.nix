{ pkgs, ... }: {
  users.extraUsers =
    let buildUser = (i: {
      "guixbuilder${i}" = {                   # guixbuilder$i
        group = "guixbuild";                  # -g guixbuild
        extraGroups = ["guixbuild"];          # -G guixbuild
        home = "/var/empty";                  # -d /var/empty
        shell = pkgs.shadow;                  # -s `which nologin`
        description = "Guix build user ${i}"; # -c "Guix buid user $i"
        isSystemUser = true;                  # --system
      };
    });

    in
      pkgs.lib.fold
        (str: acc: acc // buildUser str)
        {}
        (map (pkgs.lib.fixedWidthNumber 2)
          (builtins.genList (n: n+1) 10));

  users.extraGroups.guixbuild = {
    name = "guixbuild";
  };

  systemd.services.guix-daemon = {
    enable = true;
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
      Environment="GUIX_LOCPATH=/root/.guix-profile/lib/locale LC_ALL=en_US.utf8";
      RemainAfterExit="yes";
      StandardOutput="syslog";
      StandardError="syslog";
      TaskMax= "8192";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
