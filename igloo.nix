{ pkgs, lib, ... }:

{
  imports = [
    ./desktop.nix
    ./dvorak.nix
    ./i3.nix
    ./intel.nix
    ./kernel.nix
    ./password.nix
    ./users/mbrock
    ./wisp.nix
  ];

  os.gl = true;

  # boot.kernelPackages =
  #   pkgs.linuxKernel.packages.linux_5_16;

  fonts.fonts = with pkgs; [
    google-fonts
    iosevka
  ];

  services.xserver.enable = true;

  environment.systemPackages = with pkgs; [
    google-chrome-beta
    screen
  ];

  time.timeZone = "Europe/Riga";
  os.username = "mbrock";
  services.xserver.dpi = 200;
  networking.hostName = "igloo";

  networking.wireless.enable = true;
  networking.wireless.interfaces = ["wlp2s0"];

  networking.wireless.networks = {
    GrandPoet_Free.psk = "GRANDPOET";
    Restless.psk = "hypermedia";
  };

  location.longitude = 18.0645;
  location.latitude = 59.3328;

  nix.settings.max-jobs = lib.mkDefault 4;

  os.monospace.size = 14;

  # boot.initrd.kernelModules = ["fbcon" "ipheth"];
  boot.kernelModules = ["kvm-intel" "wl"];

  boot.kernelParams = [ "i915.enable_ips=0" "snd_hda_intel.index=1" ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

    fileSystems."/" = {
    device = "/dev/disk/by-label/root";
    fsType = "ext4";
    options = ["noatime"];
  };

  fileSystems."/boot" = {
    device = "/dev/sda1";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

#  boot.extraModulePackages = [
#    config.boot.kernelPackages.broadcom_sta
#  ];

  boot.initrd.availableKernelModules = [
    "xhci_hcd" "ehci_pci" "ahci" "usb_storage"
  ];

  boot.initrd.luks.devices.rootfs = {
    device = "/dev/sda2"; preLVM = true;
  };

  # hardware.opengl.extraPackages = [pkgs.vaapiIntel];

  services.xserver.libinput.enable = true;
  services.xserver.videoDrivers = ["intel"];

  services.xserver.deviceSection = ''
    Driver "intel"
    Option "AccelMethod" "sna"
    Option "TearFree" "true"
  '';
}
