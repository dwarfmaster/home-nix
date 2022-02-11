{ config, ... }:

let

  inherit (config.pkgsets) pkgs unstable;

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
      audacity     # sound and music editor
      pipewire     # For the pipewire CLI tools
      pulseaudio   # For the pulseaudio CLI tools
      pavucontrol  # CLI sound interface
      pamixer      # CLI sound control
    ;
    inherit (unstable)
      helvum      # GUI for pipewire
      easyeffects # GUI for effects on pipewire
    ;
    volume = volume-manager;
  };
  applications.volume = "${volume-manager}/bin/volume";
}
