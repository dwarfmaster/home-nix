{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkOption types
    mkIf concatStringsSep;
  cfg = config.pkgconfig;
in {
  options = {
    # Allow incremental setting of the PKG_CONFIG_PATH variable
    pkgconfig = {
      enable = mkEnableOption "Enable management of the PKG_CONFIG_PATH variable";
      path = mkOption {
        type = types.listOf types.path;
        description = "Paths to include in PKG_CONFIG_PATH";
        example = [ "${pkgs.gmp.dev}/lib/pkgconfig" ];
        default = [ ];
      };
    };
  };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      PKG_CONFIG_PATH = concatStringsSep ":" cfg.path;
    };
  };
}
