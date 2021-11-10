{ pkgs, config, ... }:

let keys = import ../../keys.nix;

in {
  users.users.root.openssh.authorizedKeys.keys = [keys.mbrock-ssh];

  users.users.mbrock = {
    isNormalUser = true;
    extraGroups = ["wheel" "audio"];
    openssh.authorizedKeys.keys = [keys.mbrock-ssh];
  };

  home-manager.users.mbrock = import ./profile.nix;

  services.xserver.displayManager.autoLogin.user = "mbrock";
}
