{ config, pkgs, ... }:

{
  services.tailscale.enable = true;
  services.avahi.enable = true;

  nixpkgs.config.allowUnfree = true;
  
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  services.gnome.gnome-keyring.enable = true;

  environment.systemPackages = with pkgs; [
    qutebrowser
    # firefox
    nodejs-16_x
    tdesktop
    unzip
    wget
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "libgit2-0.27.10"
  ];

  console.font = "Lat2-Terminus16";
}
