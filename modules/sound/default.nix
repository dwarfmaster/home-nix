{ config, lib, pkgs, ... }:

{
  sound = {
    enable = true;
    enableOSSEmulation = true;
  };
  hardware.pulseaudio.enable = true;
}
