{ config, ... }:

let
  inherit (config.pkgsets) pkgs unstable;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      audacity     # sound and music editor
      pipewire     # For the pipewire CLI tools
      pulseaudio   # For the pulseaudio CLI tools
      pavucontrol  # CLI sound interface
    ;
    inherit (unstable)
      helvum      # GUI for pipewire
      easyeffects # GUI for effects on pipewire
    ;
  };
}
