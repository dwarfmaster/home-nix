{
  config,
  pkgs,
  ...
}: {
  # Sync data
  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };
}
