
{ config ? { name = "Pixelfed Prod";
             url = "http://localhost";
             domain = "localhost";
             dbtype = "mysql";
             dbhost = "127.0.0.1";
             dbport = 3306;
             dbsocket = null;
             dbname = "pixelfed";
             dbuser = "pixelfed";
             dbpass = "pixelfed";
             redis_host = "127.0.0.1";
             redis_port = 6379;
             enable_mail = false;
             mail_host = "smtp.mailtrap.io";
             mail_port = 2525;
             mail_from = null;
             mail_from_name = "PixelFed";
             open_registration = false;
             mail_verification = true;
             max_users = 2000;
             enable_activity_pub = false;
             max_photo_size = 15000;
             max_caption_length = 150;
             max_album_length = 4; },
lib }:

let

  attrToString = e:
    if isNull e then "null"
    else (if builtins.isBool e then (if e then "true" else "false")
    else (if builtins.isString e then e
    else (if builtins.isInt e then builtins.toString e
    else (if builtins.isPath e then builtins.toString e
    else throw "attrToString : not convertible to string"))));

  cfg = lib.mapAttrs (name: attrToString) config;

in ''
APP_NAME="${cfg.name}"
APP_ENV=production
APP_KEY=
APP_DEBUG=false

APP_URL=${cfg.url}
APP_DOMAIN="${cfg.domain}"
ADMIN_DOMAIN="${cfg.domain}"
SESSION_DOMAIN="${cfg.domain}"
TRUST_PROXIES="*"

LOG_CHANNEL=stack

DB_CONNECTION=${cfg.dbtype}
${if isNull config.dbsocket
  then "DB_HOST=${cfg.dbhost}\nDB_PORT=${cfg.dbport}"
  else "DB_SOCKET=${cfg.dbsocket}"}
DB_DATABASE=${cfg.dbname}
DB_USERNAME=${cfg.dbuser}
DB_PASSWORD=${cfg.dbpass}

BROADCAST_DRIVER=log
CACHE_DRIVER=redis
SESSION_DRIVER=redis
QUEUE_DRIVER=redis

REDIS_SCHEME=${if isNull config.redis_port then "unix" else "tcp"}
${if isNull config.redis_port
  then "REDIS_PATH=${cfg.redis_host}"
  else "REDIS_HOST=${cfg.redis_host}"}
REDIS_PASSWORD=null
${if isNull config.redis_port
  then ""
  else "REDIS_PORT=${cfg.redis_port}"}

${if config.enable_mail
  then "MAIL_DRIVER=smtp"
  else "MAIL_DRIVER=log" }
MAIL_HOST=${cfg.mail_host}
MAIL_PORT=${cfg.mail_port}
MAIL_USERNAME=null
MAIL_PASSWORD=null
MAIL_ENCRYPTION=tls
MAIL_FROM_ADDRESS="${cfg.mail_from}"
MAIL_FROM_NAME="${cfg.mail_from_name}"

OPEN_REGISTRATION=${cfg.open_registration}
ENFORCE_EMAIL_VERIFICATION=${cfg.mail_verification}
PF_MAX_USERS=${cfg.max_users}

MAX_PHOTO_SIZE=${cfg.max_photo_size}
MAX_CAPTION_LENGTH=${cfg.max_caption_length}
MAX_ALBUM_LENGTH=${cfg.max_album_length}

ACTIVITY_PUB=${cfg.enable_activity_pub}
AP_REMOTE_FOLLOW=${cfg.enable_activity_pub}
AP_INBOX=false
PF_COSTAR_ENABLED=false

IMAGE_DRIVER="imagick"
''

