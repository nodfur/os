{ base, vmlinux, pkgs, lib }:

pkgs.writeShellScriptBin "rig-start" (
  let
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

    # vsock-server = pkgs.writeShellScriptBin "vsock-server" ''
    #   #!/usr/bin/env bash
    #   read x
    #   case "$x" in
    #     password)
    #       cat /vm/by-id/$VM_ID/password.orig
    #       ;;
    #     *)
    #       echo error
    #       ;;
    #   esac
    # '';
  in ''
    set -ex

    cd /var/lib/rigs

    mkdir -p $1
    SOCKET=$1/socket
    VSOCK=$1/vsock
    ROOT=$1/root.ext4
    rm -f $SOCKET $VSOCK

    if [ ! -f "$ROOT" ]; then
      ${pkgs.coreutils}/bin/cp ${baseImagePath} "$ROOT"
      ${pkgs.coreutils}/bin/chmod 700 "$ROOT"
    fi

    mac=$(( 0xAAFC00000000 + $1 ))
    mac_hex=$(printf "%012x" $mac | sed 's/../&:/g;s/:$//')

    exec ${pkgs.firectl}/bin/firectl \
      --firecracker-binary=${pkgs.firecracker}/bin/firecracker \
      --root-drive="$ROOT" \
      --kernel="${kernel}" \
      --kernel-opts="${kernelOpts}" \
      --socket-path="$SOCKET" \
      --memory=2048 \
      --tap-device=tap$1/"$mac_hex" \
      --metadata="{\"password\":\"$(cat /vm/by-id/$1/password.orig)\"}"
  ''
)
