{
  config,
  pkgs,
  ...
}: {
  services.korrvigs = {
    constants.curl = "${pkgs.curl}/bin/curl";
    extraModules.webcomics = ./webcomics.pl;
  };
}
