{ pkgs, lib, ... }:

let
  internet = "enp9s0";

  os-fix-apple-keyboard =
    pkgs.writeShellScriptBin "os-fix-apple-keyboard" ''
      set -ex
      file=/sys/module/hid_apple/parameters/swap_opt_cmd
      test -f $file && echo 1 > $file
    '';

in {
  imports = [
    ./mbrock.nix

    ./btrfs.nix
    ./efi.nix
    ./gmail.nix
    ./guix.nix
    ./intel.nix
    ./kernel.nix
    ./node.town.nix
    ./pi-x.nix
    ./urbit.nix
    ./1password.nix
  ];

  os.gl = true;
  os.gmail.enable = true;

  boot.kernelPackages =
    pkgs.linuxKernel.packages.linux_5_16;

  boot.initrd.availableKernelModules = [
    "xhci_pci" "ahci" "nvme" "usbhid" "uas" "sd_mod"
  ];

  system.activationScripts = {
    fixAppleKeyboard = ''
      ${os-fix-apple-keyboard}/bin/os-fix-apple-keyboard
    '';
  };

  services.xserver.enable = true;

  services.mongodb.enable = true;

  environment.systemPackages = with pkgs; [
    gdb
    calibre
    clang
    clang-tools
    emscripten
    wasmtime
    electron_16
    feh
    firefox
    foliate
    google-chrome-beta
    google-cloud-sdk
    mpv
    os-fix-apple-keyboard
    pavucontrol
    screen
    zls
    zoom-us
    evince
  ];

  home-manager.sharedModules = [{
    programs.vscode.enable = true;
  }];

  boot.initrd.kernelModules = ["amdgpu"];
  services.xserver.videoDrivers = ["amdgpu"];

  time.timeZone = "Europe/Riga";

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

  services.xserver.dpi = 160;

  networking.hostName = "chapel";
  networking.interfaces."${internet}".useDHCP = true;
  networking.nat.externalInterface = internet;

  networking.firewall.allowedTCPPorts = [1029];

  networking.wireless.enable = false;
  networking.wireless.interfaces = ["wlo1"];

  networking.wireless.networks.Restless.psk = "hypermedia";

  # services.hypocaust = {
  #   enable = true;
  #   domainName = "b14.beam.node.town";
  #   user = "mbrock";
  #   telegram.botName = "nodetownbot";
  # };

  system.stateVersion = "21.05";

  location.longitude = 18.0645;
  location.latitude = 59.3328;

  hardware.bluetooth.enable = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  nix.maxJobs = lib.mkDefault 16;
}
