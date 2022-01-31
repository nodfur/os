{ nixpkgs, isContainer } :
{ pkgs, config, lib, modulesPath, ... }:

let
  kernel = pkgs.linux_latest;

  makeDiskImage = import "${modulesPath}/../lib/make-disk-image.nix";

in {
  boot.kernelPackages = pkgs.linuxPackages_custom {
    inherit (kernel) src version;
    configfile = ./firecracker-kernel.config;
  };

  boot.isContainer = isContainer;

  boot.loader.grub.enable = false;
  fileSystems."/" = { device = "/dev/vda"; };

  services.getty.autologinUser = "root";
  services.openssh.enable = true;
  security.sudo.wheelNeedsPassword = false;

  users.users.admin = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFOnlJXoU5gKCV1s69ROeA5R4P7uZDYvIrAO6nzVqgIS admin@example.org"
    ];
  };

  environment.systemPackages = with pkgs; [
    git
  ];

  system.activationScripts = {
    installInitScript = ''
      mkdir -p /sbin
      ln -fs $systemConfig/init /sbin/init
    '';
  };

  system.build.rootfs-qemu = makeDiskImage {
    inherit pkgs config lib;
    name = "firecracker-rootfs";
    partitionTableType = "none";
    diskSize = 1024 * 60;
  };

  nix = {
    # package = pkgs.nixUnstable;
    trustedUsers = ["root" "admin"];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nix.registry.nixpkgs.flake = nixpkgs;

  environment.variables.NIX_REMOTE = lib.mkForce "";

  boot.postBootCommands =
    ''
      # After booting, register the contents of the Nix store in the Nix
      # database.
      if [ -f /nix-path-registration ]; then
        ${config.nix.package.out}/bin/nix-store --load-db \
          < /nix-path-registration &&
        rm /nix-path-registration
      fi
      # nixos-rebuild also requires a "system" profile
      ${config.nix.package.out}/bin/nix-env -p /nix/var/nix/profiles/system \
        --set /run/current-system
    '';
}
