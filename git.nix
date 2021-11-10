{
  networking.firewall.allowedTCPPorts = [
    9418
  ];

  services.gitDaemon = {
    enable = true;
    exportAll = true;
    basePath = "/srv/git";
    repositories = ["/srv/git"];
  };

  system.activationScripts = {
    setupGitRoot = ''
      mkdir -p /srv/git
      chown git /srv/git
      chgrp wheel /srv/git
      chmod 775 /srv/git
    '';
  };
}
