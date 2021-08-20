{ config, lib, pkgs, ... }:

with lib;

let

  user = "pixelfed";
  group = "pixelfed";
  home = "/data/var/lib/pixelfed";

  dbtable = "pixelfed";
  dbhost  = "/run/postgresql";
  dbtype  = "pgsql";

  fpm = config.services.phpfpm.pools.pixelfed;
  phpPackage = pkgs.php74.buildEnv {
    extensions = {enabled, all}:
      (with all;
        enabled
        ++ optional (dbtype == "pgsql") pdo_pgsql
        ++ optional (dbtype == "pgsql") pgsql
        ++ optional (dbtype == "mysql") pdo_mysql
        ++ optional (dbtype == "mysql") mysqli
        ++ [ bcmath
             ctype
             curl
             exif
             gd
             iconv
             intl
             json
             mbstring
             openssl
             redis
             tokenizer
             xml
             zip
           ]);
    extraConfig = toKeyValue phpOptions;
  };
  phpOptions = {
    post_max_size = "8M";
    file_uploads = "On";
    upload_max_filesize = "2M";
    max_file_uploads = 20;
    max_execution_time = 600;
  };
  toKeyValue = generators.toKeyValue {
    mkKeyValue = generators.mkKeyValueDefault {} " = ";
  };

  poolSettings = { };
  poolConfig   = null;

  pixelfed = with pkgs; callPackage ./pixelfed { php = phpPackage; phpPackages = php74Packages; noDev = true; };

in {
  imports = [ ./redis.nix ];

  # Database
  services.postgresql = {
    ensureDatabases = [ dbtable ];
    ensureUsers = [ {
      name = user;
      ensurePermissions."DATABASE ${dbtable}" = "ALL PRIVILEGES";
    } ];
  };

  # Packages
  environment.systemPackages = with pkgs; [
    gd
    jpegoptim
    optipng
    pngquant
    ffmpeg
    imagemagick
  ];

  # Users
  users.users.${user} = {
    home = home;
    createHome = true;
    isSystemUser = true;
    group = group;
  };
  users.groups.${group}.members = [ user config.services.nginx.user ];

  # PHP-FPM
  services.phpfpm = {
    pools.pixelfed = {
      user = user;
      group = group;
      phpPackage = phpPackage;
      phpEnv = {
        PATH = "/run/wrappers/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin:/usr/bin:/bin";
      };
      settings = mapAttrs (name: mkDefault) {
        "listen.owner" = config.services.nginx.user;
        "listen.group" = config.services.nginx.group;
      } // poolSettings;
      extraConfig = poolConfig;
    };
  };

  # NGinx
  services.nginx.enable = mkDefault true;
}
