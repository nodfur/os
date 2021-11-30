{ pkgs, lib, ... }:

let
  internet = "wlo1";

in {
  imports = [
    ./btrfs.nix
    ./desktop.nix
    ./dvorak.nix
    ./efi.nix
    ./gmail.nix
    ./i3.nix
    ./intel.nix
    ./kernel.nix
    ./node.town.nix
    ./password.nix
    ./pi-x.nix
    ./users/mbrock
    ./vnc.nix
  ];

  os.gl = true;

  os.gmail = {
    enable = true;
    address = "mikael@brockman.se";
    name = "Mikael Brockman";
    patterns = [
      "INBOX" "[Gmail]/Sent Mail" "[Gmail]/All Mail"
    ];
  };
  
  boot.kernelPackages =
    pkgs.linuxPackagesFor (pkgs.linux_latest);

  fonts.fonts = with pkgs; [
    google-fonts
    iosevka
  ];

  services.xserver.resolutions = [
    { x = 2560; y = 1440; }
    { x = 3840; y = 2160; }
  ];

  services.xserver.xrandrHeads = [{
    output = "DisplayPort-2";
    primary = true;
    # monitorConfig = ''
    #   Option "Rotate" "left"
    # '';
  }];

  environment.systemPackages = with pkgs; [
    google-chrome-beta
    _1password
    _1password-gui
    google-cloud-sdk
    zls
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

  # services.xserver.dpi = 180;

  networking.hostName = "chapel";
  networking.interfaces."${internet}".useDHCP = true;
  networking.nat.externalInterface = internet;

  networking.firewall.allowedTCPPorts = [1029];

  networking.wireless.enable = true;
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
