{ config, pkgs, ... }:

let
  inherit (pkgs) unfree unstable-unfree;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      signal-desktop   # Access signal
      element-desktop  # Chat client for matrix
      ferdi            # Meta-messenger system
      ;
    inherit (unstable-unfree)
      discord          # Audio and chat
      ;
    inherit (unfree)
      zoom-us          # Video meeting
      ;
  };
}
