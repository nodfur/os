{ pkgs, config, ... }:

{
  imports = [
    ./options.nix
    ./bash.nix
  ];

  environment.systemPackages = with pkgs; [
    (writeShellScriptBin "os-setup" ''
      set -ex

      if [ -d /os ]; then
        cd /os
        git pull origin
      else
        cd ~
        git clone https://github.com/nodfur/os
        sudo mv os /
      fi
    '')

    restless-git
  ];

  environment.interactiveShellInit = ''
    echo

    ${pkgs.figlet}/bin/figlet -f ${pkgs.figlet-fonts}/Bloody.flf \
      " ${config.networking.hostName}" \
      | ${pkgs.lolcat}/bin/lolcat
    echo
  '';

  services.openssh.enable = true;
  security.sudo.wheelNeedsPassword = false;
  programs.bcc.enable = true;
  programs.mosh.enable = true;
  services.tailscale.enable = true;

  networking.firewall.trustedInterfaces = ["tailscale0"];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  nix = {
    package = pkgs.nixUnstable;

    settings.binary-caches = [
      "https://nodfur.cachix.org"
      "https://cache.nixos.org/"
    ];

    settings.binary-cache-public-keys = [
      "nodfur.cachix.org-1:h7O80hQcd+kzNgE9i1h9yrLbnBVSQoWqSJiaM6ms5Cs="
    ];

    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    # Free up to 1GiB whenever there is less than 100MiB left.
    extraOptions = ''
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
      experimental-features = nix-command flakes
    '';
  };
}
