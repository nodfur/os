{ pkgs, config, ... }:

let keys = import ../../keys.nix;

in {
  users.users.root.openssh.authorizedKeys.keys = keys.dbrock;

  users.users.dbrock = {
    isNormalUser = true;
    extraGroups = ["wheel" "audio"];
    openssh.authorizedKeys.keys = keys.dbrock;
  };

  home-manager.users.dbrock = import ./profile.nix;

  nix.trustedUsers = ["dbrock"];
}
