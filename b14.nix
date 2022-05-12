{ pkgs, lib, ... }:

let
  internet = "enp9s0";

  my-ledger-live-desktop = with pkgs; let
    pname = "ledger-live-desktop";
    version = "2.41.3";
    name = "${pname}-${version}";
    src = fetchurl {
      url = "https://github.com/LedgerHQ/${pname}/releases/download/v${version}/${pname}-${version}-linux-x86_64.AppImage";
      hash = "sha256-Bh3wB5AAgY6l1W3UtWUHW+lJgJ0w6gw23WvEe3/Xs1g=";
    };

    appimageContents = appimageTools.extractType2 {
      inherit name src;
    };

    # Hotplug events from udevd are fired into the kernel, which then re-broadcasts them over a
    # special socket, to every libudev client listening for hotplug when the kernel does that. It will
    # try to preserve the uid of the sender but a non-root namespace (like the fhs-env) cant map root
    # to a uid, for security reasons, so the uid of the sender becomes nobody and libudev actively
    # rejects such messages. This patch disables that bit of security in libudev.
    # See: https://github.com/NixOS/nixpkgs/issues/116361
    systemdPatched = systemd.overrideAttrs ({ patches ? [ ], ... }: {
      patches = patches ++ [ ./systemd.patch ];
    });
  in
  appimageTools.wrapType2 rec {
    inherit name src;

    extraPkgs = pkgs: [ systemdPatched ];

    extraInstallCommands = ''
      mv $out/bin/${name} $out/bin/${pname}
      install -m 444 -D ${appimageContents}/ledger-live-desktop.desktop $out/share/applications/ledger-live-desktop.desktop
      install -m 444 -D ${appimageContents}/ledger-live-desktop.png $out/share/icons/hicolor/1024x1024/apps/ledger-live-desktop.png
      ${imagemagick}/bin/convert ${appimageContents}/ledger-live-desktop.png -resize 512x512 ledger-live-desktop_512.png
      install -m 444 -D ledger-live-desktop_512.png $out/share/icons/hicolor/512x512/apps/ledger-live-desktop.png
      substituteInPlace $out/share/applications/ledger-live-desktop.desktop \
        --replace 'Exec=AppRun' 'Exec=${pname}'
    '';

    meta = with lib; {
      description = "Wallet app for Ledger Nano S and Ledger Blue";
      homepage = "https://www.ledger.com/live";
      license = licenses.mit;
      maintainers = with maintainers; [ andresilva thedavidmeister nyanloutre RaghavSood th0rgal ];
      platforms = [ "x86_64-linux" ];
    };
  };


in {
  imports = [
    ./mbrock.nix
    ./1password.nix
    ./btrfs.nix
    ./efi.nix
    ./intel.nix
    ./kernel.nix
    ./node.town.nix
    ./riga.nix
    ./urbit.nix

    # ./guix.nix
    # ./pi-x.nix
    # ./printer.nix
  ];

  os.gl = true;

  boot.kernelPackages = pkgs.linuxKernel.packages.linux_5_17;

  boot.initrd.availableKernelModules = [
    "xhci_pci" "ahci" "nvme" "usbhid" "uas" "sd_mod"
  ];

  services.xserver.enable = true;

  environment.systemPackages = with pkgs; [
    electron_16
    feh
    firefox
    google-chrome-beta
    google-cloud-sdk
    mpv
    pavucontrol
    zoom-us
    my-ledger-live-desktop
  ];

  hardware.ledger.enable = true;

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

  services.vault = {
    enable = true;
    storageBackend = "file";
#    tlsCertFile = "/var/lib/acme/wisp.town/cert.pem";
#    tlsKeyFile = "/var/lib/acme/wisp.town/key.pem";
  };
}
