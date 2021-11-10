{ pkgs, ... }:

pkgs.writeShellScriptBin "rig-deploy" ''
  set -ex
  tmp=$(mktemp -d)
  id="$tmp"/id_ed25519
  cp "${./id_ed25519}" "$id"
  chmod 600 "$id"

  export NIX_SSHOPTS="-o StrictHostKeyChecking=no -i $id"
  nixos-rebuild switch --flake "/os#$1" \
    --target-host admin@"$1".local \
    --build-host localhost \
    --use-remote-sudo

  ssh $NIX_SSHOPTS admin@"$1".local test -d /src || {
    scp $NIX_SSHOPTS /srv/nixpkgs.tar admin@"$1".local:
  }

  ssh $NIX_SSHOPTS admin@"$1".local sudo-user rig-setup-user
''
