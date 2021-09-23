{ config, ... }:

let
  inherit (config.pkgsets) pkgs unfree;
in {
  home.packages = builtins.attrValues {
    inherit (pkgs)
      signal-desktop   # Access signal
      fractal          # Chat client for matrix
      nheko            # Idem
      ;
    inherit (unfree)
      discord          # Audio and chat
      zoom-us          # Video meeting
      ;
  };
}
