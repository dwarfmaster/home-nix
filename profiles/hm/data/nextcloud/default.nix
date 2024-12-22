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

    # [pair nextcloud_contacts]
    # a = "contacts_local"
    # b = "nextcloud_contacts_remote"
    # collections = [ [ "nextcloud", "nextcloud", "contacts" ] ]
    #
    # [storage contacts_local]
    # type = "filesystem"
    # path = "~/data/korrvigs-temp/calsync/korrvigs"
    # fileext = ".vcf"

  # Sync calendar and contacts
  xdg.configFile."vdirsyncer/config".text = ''
    [general]
    status_path = "${config.xdg.cacheHome}/vdirsyncer/status/"

    [storage nextcloud_contacts_remote]
    type = "carddav"
    url = "https://nextcloud.dwarfmaster.net/remote.php/dav/addressbooks/users/luc/contacts/"
    username = "luc"
    password.fetch = [ "command", "${pkgs.pass}/bin/pass", "dwarfmaster.net/nextcloud/luc" ]

    [pair nextcloud_calendars_pull]
    a = "calendars_local"
    b = "nextcloud_calendars_remote"
    collections = [ "dedukteam", "jeux", "people", "personal", "sant", "sport", "clearsy", "dance" ]
    conflict_resolution = "b wins"
    metadata = [ "color" ]

    [pair nextcloud_calendars_push]
    a = "calendars_local"
    b = "nextcloud_calendars_remote"
    collections = [ "dedukteam", "jeux", "people", "personal", "sant", "sport", "clearsy", "dance" ]
    conflict_resolution = "a wins"
    metadata = [ "color" ]

    [storage calendars_local]
    type = "filesystem"
    path = "~/data/korrvigs-temp/calsync/korrvigs/events"
    fileext = ".ics"

    [storage nextcloud_calendars_remote]
    type = "caldav"
    url = "https://nextcloud.dwarfmaster.net/remote.php/dav"
    username = "luc"
    password.fetch = [ "command", "${pkgs.pass}/bin/pass", "dwarfmaster.net/nextcloud/luc" ]
  '';
  home.packages = [pkgs.vdirsyncer];
  home.persistence."/persists/luc".directories = [
    ".cache/vdirsyncer"
  ];
}
