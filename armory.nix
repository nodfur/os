{ pkgs, ... }:

{
  imports = [
    ./ec2.nix

    ./password.nix
    ./riga.nix

    ./users/mbrock
  ];

  networking.hostName = "armory";

  nixpkgs.config.allowUnfree = true;
}
