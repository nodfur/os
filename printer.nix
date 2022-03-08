{
  networking.firewall.allowedTCPPorts = [ 631 ];
  networking.firewall.allowedUDPPorts = [ 631 ];
  services.avahi.enable = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;
  services.printing.allowFrom = ["all"];
  services.printing.browsing = true;
  services.printing.defaultShared = true;
  services.printing.drivers = [pkgs.cups-brother-hll2350dw];
  services.printing.enable = true;
  services.printing.listenAddresses = [ "*:631" ];
}
