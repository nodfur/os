{ self, nixpkgs }:

{ config, pkgs, ... }:

let
  keys = import ./keys.nix;

in {
  imports = [./hardware.nix];

  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  environment.systemPackages = with pkgs; [
    gcc
    tmux
    elixir_1_12
    erlangR24
    beamPackages.elixir_ls
    file
    firecracker
    firectl
    git
    gnumake
    ripgrep
    tailscale
    vim
    lsof
    cmake
    binutils

    nodejs-16_x
    yarn

    tdlib-json-cli

    xvfb-run
    x11vnc

    python-openai
    python3

    gnupg
    age
  ];

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
      secret-key-files = /run/node.town.pem
    '';
  };

  nix.registry.nixpkgs.flake = nixpkgs;

  services.openssh.enable = true;
  security.sudo.wheelNeedsPassword = false;

  programs.mosh.enable = true;

  networking.hostName = "hamlet";
  time.timeZone = "Europe/Riga";

  programs.bash.promptInit = ''
    if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
      PS1=$'\[\e[1m\]\h\[\e[0m\]:\w\[\e[1m\]`eval "$PS1GIT"`\[\e[0m\]\$ '
      PS1GIT='[[ `git status --short 2>/dev/null` ]] && echo \*'
      [[ $TERM = xterm ]] && PS1='\[\033]2;\h:\w\007\]'"$PS1"
    fi
  '';

  programs.bash.interactiveShellInit = ''
    for x in ~/.password-store/env/*; do
      name=$(basename "$x")
      name=''${name%.gpg}
      value=$(gpg -qd <"$x")
      echo "Found secret $name."

      # set the environment variable
      printf -v "$name" "%s" "$value"
      export "$name"
    done
  '';

  users.users.firecracker = {
    isSystemUser = true;
  };

  services.gitDaemon = {
    enable = true;
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

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/nvme0n1";

  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  system.stateVersion = "21.11";

  networking = {
    networkmanager.enable = true;
    nat.enable = true;

    firewall = {
      enable = true;
      trustedInterfaces = ["tailscale0"];
      allowedUDPPorts = [config.services.tailscale.port];
      allowedTCPPorts = [
        80      # http
        443     # https
        9418    # git server
        3389    # xrdp
      ];
      allowPing = true;
    };
  };

  services.tailscale.enable = true;

  systemd.services.tailscale-autoconnect = {
    description = "Automatic connection to Tailscale";
    after = ["network-pre.target" "tailscale.service"];
    wants = ["network-pre.target" "tailscale.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig.Type = "oneshot";
    script = ''
      sleep 2
      PATH=${pkgs.tailscale}/bin:${pkgs.jq}/bin:$PATH
      status="$(tailscale status -json | jq -r .BackendState)"
      if [ $status = "Running" ]; then
        exit 0
      fi

      authkey=$(cat /run/keys/tailscale-connect)

      tailscale up -authkey "$authkey"
    '';
  };

  services.nix-serve = {
    enable = true;
    secretKeyFile = "/run/node.town.pem";
  };

  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = hamlet
      netbios name = hamlet
      security = user
      #use sendfile = yes
      #max protocol = smb2
      hosts allow = 100.  localhost
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
      unix extensions = no
    '';
    shares = {
      homes = {
        browseable = "no";
        "read only" = "no";
        "guest ok" = "no";
        "follow symlinks" = "yes";
        "wide links" = "yes";
      };
      # public = {
      #   path = "/mnt/Shares/Public";
      #   browseable = "yes";
      #   "read only" = "no";
      #   "guest ok" = "yes";
      #   "create mask" = "0644";
      #   "directory mask" = "0755";
      #   "force user" = "username";
      #   "force group" = "groupname";
      # };
      # private = {
      #   path = "/mnt/Shares/Private";
      #   browseable = "yes";
      #   "read only" = "no";
      #   "guest ok" = "no";
      #   "create mask" = "0644";
      #   "directory mask" = "0755";
      #   "force user" = "username";
      #   "force group" = "groupname";
      # };
    };
  };
}
