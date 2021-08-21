{ config, lib, pkgs, ... }:

with lib;

let

  user = "pixelfed";
  group = "pixelfed";
  home = "/data/var/lib/pixelfed";

  dbtable = "pixelfed";
  dbhost  = "/run/postgresql";
  dbtype  = "pgsql";

  hostName = "pixelfed.dwarfmaster.net";
  domain = "dwarfmaster.net";

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
  pixelfed-config = {
    name = "DwarfMaster's Photo server";
    url = "https://${hostName}";
    domain = domain;
    dbtype = "pgsql";
    dbhost = "127.0.0.1";
    dbport = config.services.postgresql.port;
    dbname = dbtable;
    dbuser = user;
    dbpass = null;
    dbsocket = null;
    redis_host = config.services.redis.unixSocket;
    redis_port = null;
    enable_mail = true;
    mail_host = "localhost";
    mail_port = 465;
    mail_from = "pixelfed@dwarfmaster.net";
    mail_from_name = "DwarfMaster's PixelFed";
    open_registration = false;
    mail_verification = true;
    max_users = 2000;
    enable_activity_pub = true;
    max_photo_size = 15000;
    max_caption_length = 150;
    max_album_length = 10;
  };
  pixelfed-env = pkgs.writeText "env"
    (import ./pixelfed-env.nix { config = pixelfed-config; inherit lib; });

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

  # SystemdD services
  systemd.services = {
    pixelfed-setup = {
      wantedBy = [ "multi-user.target" ];
      before = [ "phpfpm-pixelfed.service"
                 "pixelfed-horizon.service"
                 "pixelfed-queue-worker.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.User = user;
      restartTriggers = [ pixelfed pixelfed-env ];
      # path = [  ];
      script = ''
        chmod og+x ${home}

        # Read-only content
        for ro in vendor routes ressources public database config bootstrap app \
                  webpack.mix.js server.php artisan; do
          ln -sf ${pixelfed}/$ro ${home}/$ro
        done

        # Data directories
        # TODO
        for dir in storage; do
          if [ ! -e ${home}/$dir ]; then
            install -o ${user} -g ${group} ${home}/$dir
          elif [ $(stat -c "%G" ${home}/$dir) != "${group}" ]; then
            chgrp -R ${group} ${home}/$dir
          fi
        done

        # Config
        ln -sf ${pixelfed-env}/env ${home}/.env

        ${phpPackage}/bin/php ${home}/artisan key:generate
        ${phpPackage}/bin/php ${home}/artisan storage:link
        ${phpPackage}/bin/php ${home}/artisan migrate --force
        ${phpPackage}/bin/php ${home}/artisan import:cities
        ${if pixelfed-config.enable_activity_pub
          then "${phpPackage}/bin/php ${home}/artisan instance:actor"
          else ""}
        ${phpPackage}/bin/php ${home}/artisan route:cache
        ${phpPackage}/bin/php ${home}/artisan view:cache
        ${phpPackage}/bin/php ${home}/artisan config:cache
        ${phpPackage}/bin/php ${home}/artisan horizon:install
        ${phpPackage}/bin/php ${home}/artisan horizon:publish
      '';
    };

    pixelfed-horizon = {
      wantedBy = [ "multi-user.target" ];
      before = [ "phpfpm-pixelfed.service" ];
      serviceConfig.Type = "simple";
      serviceConfig.User = user;
      serviceConfig.ExecStart = "${phpPackage}/bin/php ${home}/artisan horizon";
    };

    pixelfed-queue-worker = {
      wantedBy = [ "multi-user.target" ];
      before = [ "phpfpm-pixelfed.service" ];
      serviceConfig.Type = "simple";
      serviceConfig.User = user;
      serviceConfig.ExecStart = "${phpPackage}/bin/php ${home}/artisan queue:work";
    };

    pixelfed-cron = {
      serviceConfig.Type = "oneshot";
      serviceConfig.User = user;
      serviceConfig.ExecStart = "${phpPackage}/bin/php ${home}/artisan schedule:run";
    };
  };

  systemd.timers = {
    pixelfed-cron = {
      wantedBy = [ "timers.target" ];
      timerConfig.OnBootSec = "5m";
      timerConfig.OnUnitActiveSec = "5m";
      timerConfig.Unit = "pixelfed-cron.service";
    };
  };

  # NGinx
  services.nginx.enable = mkDefault true;

  services.nginx.virtualHosts.${hostName} = {
    forceSSL = true;
    enableACME = true;
    root = home;

    locations = {
      "/" = {
        priority = 100;
        tryFiles = "$uri $uri/ /index.php?$query_string";
      };

      "= /favicon.ico" = {
        priority = 200;
        extraConfig = ''
          access_log off;
          log_not_found off;
        '';
      };
      "= /robots.txt" = {
        priority = 200;
        extraConfig = ''
          access_log off;
          log_not_found off;
        '';
      };

      "~ \.php$" = {
        priority = 300;
        extraConfig = ''
          fastcgi_split_path_info ^(.+\.php)(/.+)$;
          try_files $fastcgi_script_name =404;
          fastcgi_pass unix:${config.services.phpfpm.pools.pixelfed.socket};
          fastcgi_index index.php;
          include fastcgi_params;
          fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name; # or $request_filename
        '';
      };

      "~ /\.(?!well-known).*" = {
        priority = 400;
        extraConfig = "deny all;";
      };
    };

    extraConfig = ''
      add_header X-Frame-Options "SAMEORIGIN";
      add_header X-XSS-Protection "1; mode=block";
      add_header X-Content-Type-Options "nosniff";

      index index.html index.htm index.php;

      charset utf-8;
      client_max_body_size 15M;

      error 404 /index.php;
    '';
  };
}
