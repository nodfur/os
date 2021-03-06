{ pkgs, config, ... }:

let keys = import ../../keys.nix;

in {
  users.users.root.openssh.authorizedKeys.keys = [keys.mbrock-ssh];

  users.users.mbrock = {
    isNormalUser = true;
    extraGroups = ["wheel" "audio"];
    openssh.authorizedKeys.keys = [keys.mbrock-ssh];
  };

  environment.systemPackages = with pkgs; [
    zotero
  ];

  home-manager.users.mbrock = import ./profile.nix;

  nix.settings.trusted-users = ["mbrock"];
}
