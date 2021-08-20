
{ name ? "Pixelfed Prod",
  url ? "http://localhost",
  domain ? "localhost",
  dbtype ? "mysql",
  dbhost ? "127.0.0.1",
  dbport ? 3306,
  dbsocket ? null,
  dbname ? "pixelfed",
  dbuser ? "pixelfed",
  dbpass ? "pixelfed",
  redis_host ? "127.0.0.1",
  redis_port ? 6379,
  enable_mail ? false,
  mail_host ? "smtp.mailtrap.io",
  mail_port ? 2525,
  mail_from ? null,
  mail_from_name ? "PixelFed",
  open_registration ? false,
  mail_verification ? true,
  max_users ? 2000,
  enable_activity_pub ? false,
  max_photo_size ? 15000,
  max_caption_length ? 150,
  max_album_length ? 4 }:

''
APP_NAME="${name}"
APP_ENV=production
APP_KEY=
APP_DEBUG=false

APP_URL=${url}
APP_DOMAIN="${domain}"
ADMIN_DOMAIN="${domain}"
SESSION_DOMAIN="${domain}"
TRUST_PROXIES="*"

LOG_CHANNEL=stack

DB_CONNECTION=${dbtype}
${if isNull dbsocket
  then "DB_HOST=${dbhost}\nDB_PORT=${dbport}"
  else "DB_SOCKET=${dbsocket}"}
DB_DATABASE=${dbname}
DB_USERNAME=${dbuser}
DB_PASSWORD=${dbpass}

BROADCAST_DRIVER=log
CACHE_DRIVER=redis
SESSION_DRIVER=redis
QUEUE_DRIVER=redis

REDIS_SCHEME=tcp
REDIS_HOST=${redis_host}
REDIS_PASSWORD=null
${if isNull redis_port then ""
  else "REDIS_PORT=${redis_port}"}

${if enable_mail
  then "MAIL_DRIVER=smtp"
  else "MAIL_DRIVER=log" }
MAIL_HOST=${mail_host}
MAIL_PORT=${mail_port}
MAIL_USERNAME=null
MAIL_PASSWORD=null
MAIL_ENCRYPTION=tls
MAIL_FROM_ADDRESS="${mail_from}"
MAIL_FROM_NAME="${mail_from_name}"

OPEN_REGISTRATION=${open_registration}
ENFORCE_EMAIL_VERIFICATION=${mail_verification}
PF_MAX_USERS=${max_users}

MAX_PHOTO_SIZE=${max_photo_size}
MAX_CAPTION_LENGTH=${max_caption_length}
MAX_ALBUM_LENGTH=${max_album_length}

ACTIVITY_PUB=${enable_activity_pub}
AP_REMOTE_FOLLOW=${enable_activity_pub}
AP_INBOX=false
PF_COSTAR_ENABLED=false

IMAGE_DRIVER="imagick"
''

