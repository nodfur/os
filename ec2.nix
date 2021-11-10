{ modulesPath, pkgs, ... }:
{
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;
  ec2.efi = true;
}
