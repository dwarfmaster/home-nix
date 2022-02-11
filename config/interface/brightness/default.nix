{ config, lib, pkgs, ... }:

let

  bctl = "${pkgs.brightnessctl}/bin/brightnessctl";

  brightness-manager = pkgs.writeShellScriptBin "brightness" ''
    case $1 in
      up)
       ${bctl} set 10%+
       ;;
      down)
       ${bctl} set 10%-
       ;;
      toggle)
       # TODO
       ;;
      dim)
       # TODO
       ;;
      undim)
       # TODO
       ;;
    esac
    ${config.applications.notifier} progress Brightness $(($(${bctl} get) * 100 / $(${bctl} max)))
  '';

in {
  applications.brightness = "${brightness-manager}/bin/brightness";
  home.packages = [ brightness-manager ];
}
