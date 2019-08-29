{ pkgs, ... }:

let
  pkg = pkgs.haskellPackages.greenclip;
in {
  packages = [ pkg ];
  xdg.configFile."greenclip.cfg".source = ./gc.cfg;

  systemd.user.services.greenclip = {
    Unit = {
      Description   = "ClipBoard manager daemon";
      Documentation = [ "https://github.com/erebe/greenclip" ];
    };

    Service = {
      ExecStart  = "${pkg}/bin/greenclip daemon";
      Restart    = "always";
      RestartSec = 5;
    };
  };
}

