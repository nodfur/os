{ config, pkgs, lib, ... }:

{
  boot.initrd.availableKernelModules = [
    "xhci_pci" "ahci" "nvme" "usbhid" "uas" "sd_mod"
  ];

  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
}
