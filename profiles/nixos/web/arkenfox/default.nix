{ config, lib, pkgs, ... }:

let

  inherit (lib) concatMapStrings;

  all-versions = pkgs.runCommand "arkenfox-doc-nginx" {} ''
    mkdir -p $out
    cp ${pkgs.arkenfox-vmaster-doc} $out/index.html
    cp ${pkgs.arkenfox-doc-css} $out/style.css
    ${concatMapStrings (version: "cp ${pkgs."arkenfox-v${version}-doc"} $out/${version}.html\n") lib.arkenfox.supportedVersions}
  '';

in {
  services.nginx.virtualHosts."arkenfox.dwarfmaster.net" = {
    forceSSL = true;
    enableACME = true;
    locations."/".root = "${all-versions}";
  };
}
