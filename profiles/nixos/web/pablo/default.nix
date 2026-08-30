{ pkgs, config, ... }:

let
  domain = "pablogician.net";
  directory = "/var/www/pablo";
  nginx = config.services.nginx;
  fcgi = config.services.fcgiwrap.instances.pablologician;
  user = nginx.user;
  group = config.users.users.${nginx.user}.group;
  script-dir = "/run/pablologician-update";
  update-log = "${script-dir}/log";
  secret-key = "${script-dir}/key";

  update-script = pkgs.writeShellScript "pablologician-update.sh" ''
    PATH=${pkgs.openssl}/bin:${pkgs.git}/bin:${pkgs.gawk}/bin:$PATH
    echo "[$(date --iso-8601=ns)] Start updating" >> ${update-log}
    echo "Content-Type: application/json"
    echo ""

    # Get or create secret
    if [ -f "${secret-key}" ]; then
      secret=$(cat "${secret-key}")
      secret=''${secret// /}
    else
      echo "[$(date --iso-8601=ns)] No secret found, generating a new one" >> ${update-log}
      secret=$(openssl rand -base64 32)
      touch "${secret-key}"
      chmod 600 "${secret-key}"
      echo $secret > "${secret-key}"
    fi

    # Verify github secret
    received_signature=$(cat /dev/stdin | openssl dgst -sha256 -hmac "$secret" | awk '{print $2}')
    github_signature=$(echo "$HTTP_X_HUB_SIGNATURE_256")

    if [[ "sha256=$received_signature" != "$github_signature" ]]; then
      echo "[$(date --iso-8601=ns)] Failed to check signature" >> ${update-log}
      echo '"Signature check failed"'
      exit 1
    fi

    # Update website
    cd "${directory}"
    git fetch github >/dev/null 2>&1
    git reset --hard github/main --recurse-submodule >/dev/null 2>&1

    # Echo json output
    echo "[$(date --iso-8601=ns)] Update finished" >> ${update-log}
    echo '"Success"'
  '';
in {
  services.nginx = {
    enable = true;
    virtualHosts.${domain} = {
      enableACME = true;
      forceSSL = true;
      locations."/".root = directory;
      locations."/webhook" = {
        extraConfig = ''
          fastcgi_pass unix:${fcgi.socket.address};
        '';
        fastcgiParams = {
          "HTTP_X_HUB_SIGNATURE_256" = "$http_x_hub_signature_256";
          "SCRIPT_FILENAME"= "${update-script}";
        };
      };
    };
  };

  systemd.tmpfiles.rules = [
    "d ${directory} 0770 ${user} ${group} - -"
    "d ${script-dir} 0700 ${user} ${group} - -"
  ];

  services.fcgiwrap.instances.pablologician = {
    process.user = user;
    process.group = group;
    socket.user = user;
    socket.group = group;
  };
}
