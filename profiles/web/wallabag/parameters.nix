{ wallabag, pkgs, config, ... }:

let
  # Completed from https://github.com/wallabag/wallabag/blob/master/app/config/parameters.yml.dist
  parameters = {
    database_driver       = "pdo_pgsql";
    database_host         = "127.0.0.1";
    database_port         = config.services.postgresql.port;
    database_name         = "wallabag";
    database_user         = "wallabag";
    database_password     = "3ad4e9d14669f0f1a7624b8ed72b0f98";
    database_path         = "";
    database_table_prefix = "wallabag_";
    database_socket       = "/run/postgresql/.s.PGSQL.${toString config.services.postgresql.port}";
    database_charset      = "utf8";

    domain_name = "https://reading.dwarfmaster.com";
    server_name = "DwarfMaster's wallabag instance";

    mailer_transport  = "smtp";
    mailer_user       = "wallabag@dwarfmaster.net";
    mailer_password   = "";
    mailer_host       = "127.0.0.1";
    mailer_port       = 587;
    mailer_encryption = "tls";
    mailer_auth_mode  = "plain";

    locale = "en_IE";

    # A secret key that's used to generate certain security-related tokens
    # TODO Move out of configuration
    secret = "e25900dd80a81da217891884946452191b5d6ac1315125579002891893d3871808cad04dc3b54c723977802557852118e2ef213a977eb6658175a9ee7b69279a";

    # two factor stuff
    twofactor_auth   = true;
    twofactor_sender = "wallabag@dwarfmaster.net";

    # fosuser stuff
    fosuser_registration = true;
    fosuser_confirmation = true;

    # how long the access token should live in seconds for the API
    fos_oauth_server_access_token_lifetime  = 3600;
    # how long the refresh token should life in seconds for the API
    fos_oauth_server_refresh_token_lifetime = 1209600;

    from_email = "wallabag@dwarfmaster.net";
    rss_limit  = 50;

    # RabbitMQ processing
    rabbitmq_host           = "localhost";
    rabbitmq_port           = config.services.rabbitmq.port;
    rabbitmq_user           = "guest";
    rabbitmq_password       = "guest";
    rabbitmq_prefetch_count = 10;

    # Redis processing
    redis_scheme   = "tcp";
    redis_host     = "localhost";
    redis_port     = config.services.redis.port;
    redis_path     = null;
    redis_password = null;

    # sentry logging
    sentry_dsn = "";
  };

  parameters-json = pkgs.writeTextFile { name = "parameters.json"; text = builtins.toJSON { inherit parameters; }; };
in pkgs.runCommand
    "parameters.yml" { preferLocalBuild = true; } ''
      mkdir -p $out/config
      ${pkgs.remarshal}/bin/json2yaml -i ${parameters-json} -o $out/config/parameters.yml
    ''
