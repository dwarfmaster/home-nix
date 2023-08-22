{
  lib,
  config,
  ...
}: let
  inherit (lib) types;
in {
  options = {
    hardware.specs = {
      cores = lib.mkOption {
        description = "Number of physical cores available";
        type = types.int;
        default = 4;
      };

      threads = lib.mkOption {
        description = "Number of threads available";
        type = types.int;
        default = config.hardware.specs.cores * 2;
      };

      kvm = lib.mkEnableOption "Machine has kvm support";

      battery = lib.mkOption {
        description = "Name of the battery device";
        type = types.nullOr types.str;
        default = null;
      };

      wifiDevice = lib.mkOption {
        description = "Name of the wifi device";
        type = types.nullOr types.str;
        default = null;
      };

      backlightDevice = lib.mkOption {
        description = "Name of the backlight device";
        type = types.nullOr types.str;
        default = null;
      };
    };
  };
}
