{ lib, ... }:
{
  imports = [
    ../luc-common
  ];

  home-manager.users.luc.imports = [ lib.hmConfigurations.luc-persist ];
}
