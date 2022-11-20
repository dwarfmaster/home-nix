{ lib, config, ... }:

let
  inherit (lib) types;
in {
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
  };
}
