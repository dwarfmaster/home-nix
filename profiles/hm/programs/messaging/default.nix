{
  config,
  pkgs,
  ...
}: let
  inherit (pkgs) unfree;
in {
  home.packages = builtins.attrValues {
    inherit
      (pkgs)
      signal-desktop # Access signal
      element-desktop # Chat client for matrix
      # ferdi            # Meta-messenger system
      ;
    inherit
      (unfree)
      discord # Audio and chat
      zoom-us # Video meeting
      ;
  };
}
