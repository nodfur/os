{ pkgs, lib, ... }:

{
  boot.kernelPackages = pkgs.linux_5_16;

  nixpkgs = {
    overlays = [
      (self: super: {
        linux_5_16 = pkgs.linuxPackagesFor (pkgs.linux_latest.override {
          argsOverride = rec {
            src = pkgs.linux-src;
            version = "5.16.0";
          };
        });
      })
    ];
  };

  boot.initrd.availableKernelModules = [
    "xhci_pci" "ahci" "nvme" "usbhid" "uas" "sd_mod"
  ];

  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
}
