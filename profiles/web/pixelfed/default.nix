{ config, lib, pkgs, ... }:

with lib;

let

  user = "pixelfed";
  group = "pixelfed";
  home = "/var/www/pixelfed";

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
  } // {
    short_open_tag = "Off";
    expose_php = "Off";
    error_reporting = "E_ALL & ~E_DEPRECATED & ~E_STRICT";
    display_errors = "stderr";
    "opcache.enable_cli" = "1";
    "opcache.interned_strings_buffer" = "8";
    "opcache.max_accelerated_files" = "10000";
    "opcache.memory_consumption" = "128";
    "opcache.revalidate_freq" = "1";
    "opcache.fast_shutdown" = "1";
    "openssl.cafile" = "/etc/ssl/certs/ca-certificates.crt";
    catch_workers_output = "yes";
  };
  toKeyValue = generators.toKeyValue {
    mkKeyValue = generators.mkKeyValueDefault {} " = ";
  };

  poolSettings = {
    "pm" = "dynamic";
    "pm.max_children" = "32";
    "pm.start_servers" = "2";
    "pm.min_spare_servers" = "2";
    "pm.max_spare_servers" = "4";
    "pm.max_requests" = "500";
  };
  poolConfig   = null;

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
    redis_host = "127.0.0.1";
    redis_port = config.services.redis.port;
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
  pixelfed = with pkgs; callPackage ./pixelfed {
    storage = home;
    envConfig = pixelfed-config;
    php = phpPackage;
    phpPackages = php74Packages;
    noDev = true;
  };

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
    group = group;
    createHome = true;
    isSystemUser = true;
    extraGroups = [ "redis" ];
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
  # TODO
  systemd.services = {
    pixelfed-setup = {
      wantedBy = [ "multi-user.target" ];
      before = [ "phpfpm-pixelfed.service" ];
                 #"pixelfed-horizon.service"
                 #"pixelfed-queue-worker.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.User = user;
      restartTriggers = [ pixelfed ];
      # path = [  ];
      script = ''
        chmod 775 ${home}

        if [ ! -e ${home}/storage ]; then
          install -o ${user} -g ${group} -m 0775 -d ${home}/storage
        fi
        for path in framework framework/sessions framework/cache framework/views; do
          if [ ! -e $path ]; then
            install -o ${user} -g ${group} -m 0775 -d ${home}/storage/$path
          fi
        done

        for path in app bootstrap config database resources routes tests vendor \
                    server.php composer.json; do
          if [ -e ${home}/$path ]; then
            rm ${home}/$path
          fi
          ln -s ${pixelfed}/$path ${home}/$path
        done

        for path in .env public; do
          if [ -e ${home}/$path ]; then
            rm -rf ${home}/$path
          fi
          if [ -d ${pixelfed}/$path ]; then
            # TODO
            install -o ${user} -g ${group} -m 0775 -d ${home}/$path
            for file in ${pixelfed}/$path/*; do
              ln -s $file ${home}/$path/$(basename $file)
            done
          else
            install -o ${user} -g ${group} -m 0665 ${pixelfed}/$path ${home}/$path
          fi
        done

        ${phpPackage}/bin/php ${pixelfed}/artisan key:generate
        ${phpPackage}/bin/php ${pixelfed}/artisan migrate --force
        ${phpPackage}/bin/php ${pixelfed}/artisan import:cities
        ${if pixelfed-config.enable_activity_pub
          then "${phpPackage}/bin/php ${pixelfed}/artisan instance:actor"
          else ""}
        ${phpPackage}/bin/php ${pixelfed}/artisan route:cache
        ${phpPackage}/bin/php ${pixelfed}/artisan view:cache
        ${phpPackage}/bin/php ${pixelfed}/artisan config:cache
        ${phpPackage}/bin/php ${pixelfed}/artisan horizon:install
        ${phpPackage}/bin/php ${pixelfed}/artisan horizon:publish
      '';
    };

  #   pixelfed-horizon = {
  #     wantedBy = [ "multi-user.target" ];
  #     before = [ "phpfpm-pixelfed.service" ];
  #     serviceConfig.Type = "simple";
  #     serviceConfig.User = user;
  #     serviceConfig.ExecStart = "${phpPackage}/bin/php ${home}/artisan horizon";
  #   };

  #   pixelfed-queue-worker = {
  #     wantedBy = [ "multi-user.target" ];
  #     before = [ "phpfpm-pixelfed.service" ];
  #     serviceConfig.Type = "simple";
  #     serviceConfig.User = user;
  #     serviceConfig.ExecStart = "${phpPackage}/bin/php ${home}/artisan queue:work";
  #   };

  #   pixelfed-cron = {
  #     serviceConfig.Type = "oneshot";
  #     serviceConfig.User = user;
  #     serviceConfig.ExecStart = "${phpPackage}/bin/php ${home}/artisan schedule:run";
  #   };
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
    # TODO
    forceSSL = false;
    enableACME = false;
    root = "${pixelfed}/public";

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
          include ${config.services.nginx.package}/conf/fastcgi.conf;
          fastcgi_split_path_info ^(.+\.php)(/.+)$;
          try_files $fastcgi_script_name =404;
          fastcgi_pass unix:${config.services.phpfpm.pools.pixelfed.socket};
          fastcgi_index index.php;
          fastcgi_param PATH_INFO $path_info;
          fastcgi_param HTTPS on;
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

      error_page 404 /index.php;
    '';
  };
}
