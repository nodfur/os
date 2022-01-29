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
    ./intel.nix
    ./kernel.nix
    ./node.town.nix
    ./pi-x.nix
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
    pkgs.linuxKernel.packages.linux_5_16;

  fonts.fonts = with pkgs; [
    google-fonts
    iosevka
  ];

  system.activationScripts = {
    fixAppleKeyboard = ''
      ${os-fix-apple-keyboard}/bin/os-fix-apple-keyboard
    '';
  };

  services.xserver.enable = true;
#  services.xserver.resolutions = [
#    { x = 3840; y = 2160; }
#    { x = 2560; y = 1440; }
#  ];

#  services.xserver.xrandrHeads = [
#    {
#      output = "DisplayPort-3";
#      monitorConfig = ''
#        Option "Rotate" "right"
#      '';
#    }
#    {
#      primary = true;
#      output = "DisplayPort-5";
#    }
#  ];

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
  ];

  users.extraUsers =
    let buildUser = (i: {
      "guixbuilder${i}" = {                   # guixbuilder$i
        group = "guixbuild";                  # -g guixbuild
        extraGroups = ["guixbuild"];          # -G guixbuild
        home = "/var/empty";                  # -d /var/empty
        shell = pkgs.nologin;                 # -s `which nologin`
        description = "Guix build user ${i}"; # -c "Guix buid user $i"
        isSystemUser = true;                  # --system
      };
    });

    in
      pkgs.lib.fold
        (str: acc: acc // buildUser str)
        {}
        (map (pkgs.lib.fixedWidthNumber 2)
          (builtins.genList (n: n+1) 10));

  users.extraGroups.guixbuild = {
    name = "guixbuild";
  };

  systemd.services.guix-daemon = {
    enable = true;
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
      Environment="GUIX_LOCPATH=/root/.guix-profile/lib/locale LC_ALL=en_US.utf8";
      RemainAfterExit="yes";
      StandardOutput="syslog";
      StandardError="syslog";
      TaskMax= "8192";
    };
    wantedBy = [ "multi-user.target" ];
  };

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

  services.xserver.dpi = 200;

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
