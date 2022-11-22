{
  config,
  lib,
  pkgs,
  ...
}: {
  services.grocy = {
    enable = true;
    hostName = "grocy.dwarfmaster.net";
    settings = {
      currency = "EUR";
      culture = "fr";
      calendar = {
        showWeekNumber = true;
        firstDayOfWeek = 0;
      };
    };
  };
}
