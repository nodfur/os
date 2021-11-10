# In order to create Firecracker root file systems more efficiently,
# we want to upgrade a premade Firecracker root file system image that
# we can make using qemu.

{
  rig-start, base, vmlinux, pkgs, config, lib, ...
}:

let
  rig-start-bin = rig-start {
    inherit base vmlinux pkgs lib;
  };

  sshKey = pkgs.writeTextFile {
    name = "id_ed25519";
    text = ''
      -----BEGIN OPENSSH PRIVATE KEY-----
      b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
      QyNTUxOQAAACBTp5SV6FOYCgldbOvUTngOUeD+7mQ2LyKwDup81aoCEgAAAJDFN4l5xTeJ
      eQAAAAtzc2gtZWQyNTUxOQAAACBTp5SV6FOYCgldbOvUTngOUeD+7mQ2LyKwDup81aoCEg
      AAAECxSRlWN+AEm40f8KWTMyk7d4AYpOXjj0jwor33JsSsVlOnlJXoU5gKCV1s69ROeA5R
      4P7uZDYvIrAO6nzVqgISAAAADW1icm9ja0BjaGFwZWw=
      -----END OPENSSH PRIVATE KEY-----
    '';
  };

  mkKernelOpts = opts:
    lib.concatStringsSep " "
      (lib.mapAttrsToList
        (k: v: if v == true then k else "${k}=${toString v}") opts);
  initPath = "/nix/var/nix/profiles/system/init";
  baseImagePath = "${base}/nixos.img";
  kernel =
    "${vmlinux}/vmlinux";
  kernelOpts = mkKernelOpts {
    init = initPath;
    console = "ttyS0";
    reboot = "k";
    panic = 1;
    pci = "off";
    quiet = true;
    loglevel = 1;
  };
''
    set -ex

  ''
)


in pkgs.runCommand "firecracker-image"
  {
    buildInputs = with pkgs; [
      coreutils firectl socat
    ];
  }
  ''
    mkdir $out
    SOCKET=socket
    ROOT=root.ext4

    cp ${baseImagePath} "$ROOT"
    chmod 700 "$ROOT"

    firectl \
      --firecracker-binary=${pkgs.firecracker}/bin/firecracker \
      --root-drive="$ROOT" \
      --kernel="${kernel}" \
      --kernel-opts="${kernelOpts}" \
      --socket-path="$SOCKET" \
      --vsock-device=vsock:2 \
      --memory=4096 & pid=$!

    # wait for a connection on port 1 of the vsock
    echo ok | socat - UNIX-LISTEN:./vsock_1

    # tunnel sshd from the guest
    socat TCP4-LISTEN

    # now we can ssh via the vsock
    ssh -f ${sshKey} admin@vm1.local uname -a > $out
  ''
