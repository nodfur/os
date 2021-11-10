{ pkgs, ... }:

pkgs.writeShellScriptBin "rig-setup-user" ''
  set -ex

  if [ ! -d /src ]; then
    mkdir -p ~/src
    cd ~/src

    git clone --recursive git://root.node.town/os

    mkdir nixpkgs
    cd nixpkgs
    sudo mv /home/admin/nixpkgs.tar /tmp/
    tar xvf /tmp/nixpkgs.tar
    sudo rm -rf /tmp/nixpkgs.tar
    git remote set-url origin git://root.node.town/nixpkgs

    cd /
    sudo mv ~/src .
  fi

  cd /src/os
  git fetch origin
  git reset --hard origin/main

  cd /src/nixpkgs
  git fetch origin
  git checkout restless
  hash1=$(git rev-parse HEAD)
  hash2=$(git rev-parse origin/restless)
  if [[ $hash1 != $hash2 ]]; then
    git reset --hard origin/restless
    cd /src/os
    nix flake lock --update-input nixpkgs
  fi

  sudo nix-collect-garbage -d
''
