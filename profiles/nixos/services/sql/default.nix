{ pkgs, config, ... }:

let
  cfg = config.services.postgresql;
  newPostgres = pkgs.postgresql_14;
  psqlData = schema: "/data/var/lib/postgresql/${schema}";
  # Taken from https://nixos.org/manual/nixos/stable/#module-services-postgres-upgrading
  update-pg = pkgs.writeScriptBin "upgrade-pg-cluster" ''
      set -eux
      # XXX it's perhaps advisable to stop all services that depend on postgresql
      systemctl stop postgresql

      export NEWDATA="${psqlData newPostgres.psqlSchema}"

      export NEWBIN="${newPostgres}/bin"

      export OLDDATA="${cfg.dataDir}"
      export OLDBIN="${cfg.package}/bin"

      install -d -m 0700 -o postgres -g postgres "$NEWDATA"
      cd "$NEWDATA"
      sudo -u postgres $NEWBIN/initdb -D "$NEWDATA"

      sudo -u postgres $NEWBIN/pg_upgrade \
        --old-datadir "$OLDDATA" --new-datadir "$NEWDATA" \
        --old-bindir $OLDBIN --new-bindir $NEWBIN \
        "$@"
  '';
in {
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_14;
    dataDir = psqlData cfg.package.psqlSchema;
  };
  environment.systemPackages = 
    if newPostgres.psqlSchema != cfg.package.psqlSchema
      then [ update-pg ]
      else [ ];
}
