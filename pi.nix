{ pkgs, ... }:
{
#  boot.kernelPackages = pkgs.linuxPackages_rpi4;

  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };

  boot = {
    tmpOnTmpfs = true;
    initrd.availableKernelModules = ["usbhid" "usb_storage"];
    kernelParams = [
      "8250.nr_uarts=1"
      "console=ttyAMA0,115200"
      "console=tty1"
      "cma=128M"
    ];
  };

  networking = {
    wireless = {
      enable = true;
      networks.Restless.psk = "hypermedia";
      interfaces = ["wlan0"];
    };
  };

  hardware.enableRedistributableFirmware = true;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  powerManagement.cpuFreqGovernor = "ondemand";
}
