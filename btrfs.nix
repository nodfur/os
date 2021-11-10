{
  boot.supportedFilesystems = ["btrfs"];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/89d23891-5347-48f7-abdf-e9361090aac1";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/89d23891-5347-48f7-abdf-e9361090aac1";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/6258-7DA0";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/5e4ad537-a42f-43ae-98da-56f1601b5f09"; }
    ];
}
