{ config, ... }:

{
  networking = {
    hostName = ""; # set from dhcp
    dhcpcd.enable = true;
    firewall.allowPing = true;
    useHostResolvConf = false;
    usePredictableInterfaceNames = false;
    enableIPv6 = false;
    interfaces.eth0.useDHCP = true;

    hosts = {
      "172.16.${toString config.firecracker.guestIndex}.1" = [
        "root.node.town"
      ];
    };
  };
}
