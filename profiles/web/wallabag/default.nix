{ config, lib, pkgs, ... }:

# Inspired by
# https://code.lakhwara.com/Anish/nix-garden/src/commit/10ebee18b86ba698e678464a03aef8a2fcbdb71c/modules/mod-wallabag.nix

let
  pool = config.services.phpfpm.pools.wallabag;
  wallabag = pkgs.wallabag;
  parameters = import ./parameters.nix { inherit wallabag pkgs config; };
  appDir = pkgs.buildEnv {
    name = "wallabag-app-dir";
    ignoreCollisions = true;
    checkCollisionContents = false;
    paths = [ parameters "${wallabag}/app" ];
  };
  dataDir = "/data/var/www/wallabag";
  php = pkgs.php74;
  exts = pkgs.php74Extensions;
  phpPkgs = pkgs.php74Packages;

  # See there for available commands:
  # https://doc.wallabag.org/en/admin/console_commands.html
  # A user can be made admin with the fos:user:promote --super <user> command
  console = pkgs.writeShellScriptBin "wallabag-console" ''
      export WALLABAG_DATA="${dataDir}"
      cd "${dataDir}"
      ${php}/bin/php ${wallabag}/bin/console --env=prod $@
    '';
in {
  # Wallabag config yml files needs to be recreated at each update, and
  # var/cache needs to be cleared between restart
  assertions = [ {
    assertion = wallabag.version == "2.4.3";
    message   = "Wallabag update to ${wallabag.version} needs manual intervention";
  } ];

  # Install console manager
  environment.systemPackages = [ console ];

  # Inspired by https://doc.wallabag.org/ens/admin/installation/virtualhosts.html
  services.nginx.virtualHosts."reading.dwarfmaster.net" = {
    forceSSL = true;
    enableACME = true;

    root = "${wallabag}/web";
    locations."/" = {
      priority = 10;
      tryFiles = "$uri /app.php$is_args$args";
    };
    locations."~ ^/app\\.php(/|$)" = {
      priority = 100;
      fastcgiParams = {
        SCRIPT_FILENAME = "$realpath_root$fastcgi_script_name";
        DOCUMENT_ROOT   = "$realpath_root";
      };
      extraConfig = ''
          fastcgi_pass unix:${pool.socket};
          include ${config.services.nginx.package}/conf/fastcgi_params;
          include ${config.services.nginx.package}/conf/fastcgi.conf;
          internal;
        '';
    };
    locations."~ \\.php$" = {
      priority = 1000;
      return = "404";
    };

    extraConfig = ''
      error_log /var/log/nginx/wallabag_error.log;
      access_log /var/log/nginx/wallabag_access.log;
    '';
  };

  # PHP
  services.redis.enable = true;
  services.phpfpm.pools.wallabag = {
    user = "wallabag";
    group = "wallabag";
    phpPackage = php;
    phpEnv = {
      WALLABAG_DATA = "/data/var/www/wallabag";
      PATH = lib.makeBinPath [ php ];
    };
    settings = {
      "listen.owner" = config.services.nginx.user;
      "pm"                   = "dynamic";
      "pm.max_children"      = 32;
      "pm.max_requests"      = 500;
      "pm.start_servers"     = 1;
      "pm.min_spare_servers" = 1;
      "pm.max_spare_servers" = 5;
      "php_admin_value[error_log]" = "stderr";
      "php_admin_flag[log_errors]" = true;
      "catch_workers_output"       = true;
    };
    phpOptions = ''
        extension=${exts.pdo}/lib/php/extensions/pdo.so
        extension=${exts.pdo_pgsql}/lib/php/extensions/pdo_pgsql.so
        extension=${exts.session}/lib/php/extensions/session.so
        extension=${exts.ctype}/lib/php/extensions/ctype.so
        extension=${exts.dom}/lib/php/extensions/dom.so
        extension=${exts.simplexml}/lib/php/extensions/simplexml.so
        extension=${exts.gd}/lib/php/extensions/gd.so
        extension=${exts.mbstring}/lib/php/extensions/mbstring.so
        extension=${exts.xml}/lib/php/extensions/xml.so
        extension=${exts.tidy}/lib/php/extensions/tidy.so
        extension=${exts.iconv}/lib/php/extensions/iconv.so
        extension=${exts.curl}/lib/php/extensions/curl.so
        extension=${exts.gettext}/lib/php/extensions/gettext.so
        extension=${exts.tokenizer}/lib/php/extensions/tokenizer.so
        extension=${exts.bcmath}/lib/php/extensions/bcmath.so
        extension=${exts.intl}/lib/php/extensions/intl.so
        extension=${exts.opcache}/lib/php/extensions/opcache.so
      '';
  };

  # PostgreSQL Database
  services.postgresql = {
    ensureDatabases = [ "wallabag" ];
    # Wallabag does not support passwordless login into database,
    # so the database password for the user must be manually set
    ensureUsers = [
      { name = "wallabag";
        ensurePermissions."DATABASE wallabag" = "ALL PRIVILEGES";
      }
    ];
  };

  # Data directory
  systemd.tmpfiles.rules = let
    user = "wallabag";
  in [ "d ${dataDir} 0700 ${user} ${user} - -" ];
  systemd.services."wallabag-setup" = {
    description = "Wallabag install service";
    wantedBy = [ "multi-user.target" ];
    before = [ "phpfpm-wallabag.service" ];
    requiredBy = [ "phpfpm-wallabag.service" ];
    after = [ "postgresql.service" ];
    path = [ pkgs.coreutils php phpPkgs.composer ];

    serviceConfig = {
      User = "wallabag";
      Group = "wallabag";
      Type = "oneshot";
      RemainAfterExit = "yes";
      PermissionsStartOnly = true;
      Environment="WALLABAG_DATA=${dataDir}";
    };

    script = ''
        echo "Setting up wallabag files in ${dataDir} ..."
        cd "${dataDir}"

        rm -rf var/cache/*
        rm -f app
        ln -sf ${appDir} app
        ln -sf ${wallabag}/composer.{json,lock} .

        if [ ! -f installed ]; then
          echo "Installing wallabag"
          php ${wallabag}/bin/console --env=prod wallabag:install --no-interaction
          touch installed
        else
          php ${wallabag}/bin/console --env=prod doctrine:migrations:migrate --no-interaction
        fi
        php ${wallabag}/bin/console --env=prod cache:clear
      '';
  };

  # Misc settings
  services.rabbitmq.enable = true;
  users.users.wallabag = {
    isSystemUser = true;
    group = "wallabag";
  };
  users.groups.wallabag = { };
}
