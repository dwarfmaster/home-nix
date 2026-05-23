{ ... }:

let
  blogDomain = "blog.dwarfmaster.net";
in {
  programs.korrvigs.server.nginx = {
    enable = true;
    passwordHash = "$2y$05$pVEpea9udNCn5B7msH9YW.dk2L2rHgHE5.2id0.dwzYz3YNSnGIW.";
    domain = "korrvigs.dwarfmaster.net";
    staticDomain = "korrvigs-static.dwarfmaster.net";
  };

  programs.korrvigs.autorun = {
    enable = true;
    timeWindow = 3 * 3600;
  };

  programs.korrvigs.blog = {
    enable = true;
    directory = "/var/www/blog";
    domain = blogDomain;
    nginx = true;
    autopublish = "Mon *-*-* 3:00";
  };

  services.nginx.virtualHosts.${blogDomain}.serverAliases =
    [ "dwarfmaster.net" "www.dwarfmaster.net" ];
}
