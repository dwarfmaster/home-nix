{ config, lib, ... }:

# TODO setting locking by session on loginctl event
let
  inherit (config.pkgsets) pkgs;

  physlock = "${pkgs.physlock}/bin/physlock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";
  vlock = "${pkgs.vlock}/bin/vlock";

  locker = lib.writeShellScript "lock-script"
    ''
    case $(tty) in
      /dev/tty*)
        ${vlock}
      ;;
      *)
        ${physlock} -l
        ${i3lock}
        ${physlock} -L
      ;;
    esac
    '';
in {
  home.packages = [ pkgs.physlock pkgs.vlock pkgs.i3lock ];
}
