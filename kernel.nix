{ pkgs, lib, ... }:

{
  boot.kernelPackages =
    pkgs.linuxPackagesFor pkgs.linux_latest;

  boot.initrd.availableKernelModules = [
    "xhci_pci" "ahci" "nvme" "usbhid" "uas" "sd_mod"
  ];

  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
}
