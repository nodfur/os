{ config, pkgs, lib, ... }:

{
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
}
