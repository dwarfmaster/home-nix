{
  config,
  lib,
  pkgs,
  ...
}: {
  services.influxdb.enable = true;

  services.grafana = {
    enable = true;
    provision.enable = true;
    provision.datasources = [
      {
        name = "Finance";
        type = "influxdb";
        url = "http://localhost:8086";
        access = "proxy";
        database = "finance";
        basicAuthUser = "root";
        basicAuthPassword = "root";
      }
    ];
  };
}
