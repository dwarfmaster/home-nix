{ config, pkgs, ... }:

let

  inherit (pkgs) unstable;

  pamixer = "${pkgs.pamixer}/bin/pamixer";

  # TODO icons for volume notifications
  volume-manager = pkgs.writeShellScriptBin "volume" ''
    case $1 in
      up)
        ${pamixer} -i 5
        ${pamixer} -u
       ;;
      down)
        ${pamixer} -d 5
        ${pamixer} -u
       ;;
      toggle)
        ${pamixer} -t
       ;;
      mute)
        ${pamixer} -m
       ;;
      unmute)
        ${pamixer} -u
       ;;
    esac
    if [[ $(${pamixer} --get-mute) = "true" ]]; then
      ${config.applications.notifier} normal Volume Muted
    else
      ${config.applications.notifier} progress Volume $(${pamixer} --get-volume)
    fi
  '';
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      alsaUtils    # Sound card control
      audacity     # sound and music editor
      pamixer      # CLI sound control
      pavucontrol  # CLI sound interface
      pipewire     # For the pipewire CLI tools
      pulseaudio   # For the pulseaudio CLI tools
    ;
    inherit (unstable)
      helvum      # GUI for pipewire
      # TODO doesn't work on aarch64
      # easyeffects # GUI for effects on pipewire
    ;
    volume = volume-manager;
  };
  applications.volume = "${volume-manager}/bin/volume";
}
