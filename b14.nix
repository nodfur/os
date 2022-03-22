{ pkgs, lib, ... }:

let
  internet = "enp9s0";

in {
  imports = [
    ./mbrock.nix

    ./1password.nix
    ./btrfs.nix
    ./efi.nix
    ./gmail.nix
    ./guix.nix
    ./intel.nix
    ./kernel.nix
    ./node.town.nix
    ./pi-x.nix
    ./printer.nix
    ./riga.nix
    ./urbit.nix
  ];

  os.gl = true;
  os.gmail.enable = true;

  boot.kernelPackages =
    pkgs.linuxKernel.packages.linux_5_16;

  boot.initrd.availableKernelModules = [
    "xhci_pci" "ahci" "nvme" "usbhid" "uas" "sd_mod"
  ];

  services.xserver.enable = true;

  environment.systemPackages = with pkgs; [
    calibre
    electron_16
    evince
    feh
    firefox
    gdb
    google-chrome-beta
    google-cloud-sdk
    mpv
    pavucontrol
    screen
    zls
    zoom-us
  ];

  home-manager.sharedModules = [{
    programs.vscode.enable = true;
  }];

  boot.initrd.kernelModules = ["amdgpu"];
  services.xserver.videoDrivers = ["amdgpu"];

  os.username = "mbrock";
  os.vnc.size.height = 900;
  os.vnc.size.width = 1440;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  services.xserver.dpi = 180;

  networking.hostName = "chapel";
  networking.interfaces."${internet}".useDHCP = true;
  networking.nat.externalInterface = internet;

  networking.firewall.allowedTCPPorts = [1029];

  networking.wireless.enable = false;
  networking.wireless.interfaces = ["wlo1"];
  networking.wireless.networks.Restless.psk = "hypermedia";

  system.stateVersion = "21.05";

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  nix.settings.max-jobs = lib.mkDefault 16;
}
