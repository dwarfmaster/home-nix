{ lib, config, ... }:
{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc = {
    imports = [ lib.hmConfigurations.luc-persist ];

    hardware.specs = {
      cores = config.hardware.specs.cores;
      threads = config.hardware.specs.threads;
    };
  };
}
