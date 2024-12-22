{
  pkgs,
  config,
  ...
}: let
  cfg = config.services.postgresql;
in {
  services.postgresql = {
    extraPlugins = [ cfg.package.pkgs.postgis ];
    ensureDatabases = [ "korrvigs" ];
    ensureUsers = [
      { name = "luc"; }
    ];
  };
}
