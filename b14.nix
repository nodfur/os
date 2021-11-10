{ pkgs, lib, ... }:

let
  internet = "wlo1";

in {
  imports = [
    ./kernel.nix
    ./intel.nix
    ./btrfs.nix
    ./efi.nix
    ./dvorak.nix
    
    ./password.nix
    ./desktop.nix
    ./vnc.nix

    ./i3.nix
    
    ./node.town.nix

    ./users/mbrock

    # ./hypocaust.nix
    #./grafana.nix
    # ./ssbot.nix

    ./pi-x.nix
  ];

  environment.systemPackages = with pkgs; [
    google-chrome-beta
    _1password
    _1password-gui
  ];
 
  boot.initrd.kernelModules = ["amdgpu"];
  services.xserver.videoDrivers = ["amdgpu"];

  time.timeZone = "Europe/Riga";
  
  os.vnc.size.height = 900;
  os.vnc.size.width = 1440;
  os.username = "mbrock";

  services.xserver.dpi = 180;

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
