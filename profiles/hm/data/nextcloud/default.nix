{ config, pkgs, ... }:

{
  # Sync data
  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };

  # Sync calendar and contacts
  xdg.configFile."vdirsyncer/config".text = ''
  [general]
  status_path = "${config.xdg.cacheHome}/vdirsyncer/status/"

  [pair nextcloud_contacts]
  a = "nextcloud_contacts_local"
  b = "nextcloud_contacts_remote"
  collections = [ [ "nextcloud", "nextcloud", "contacts" ] ]
  conflict_resolution = "a wins"

  [storage nextcloud_contacts_local]
  type = "filesystem"
  path = "~/data/annex/contacts/"
  fileext = ".vcf"

  [storage nextcloud_contacts_remote]
  type = "carddav"
  url = "https://nextcloud.dwarfmaster.net/remote.php/dav/addressbooks/users/luc/contacts/"
  username = "luc"
  password.fetch = [ "command", "${pkgs.pass}/bin/pass", "dwarfmaster.net/nextcloud/luc" ]
  '';
  home.packages = [ pkgs.vdirsyncer ];
}
