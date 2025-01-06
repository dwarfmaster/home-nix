{ ... }:
{
  programs.korrvigs.server.nginx = {
    enable = true;
    passwordHash = "$2y$05$pVEpea9udNCn5B7msH9YW.dk2L2rHgHE5.2id0.dwzYz3YNSnGIW.";
    domain = "korrvigs.dwarfmaster.net";
    staticDomain = "korrvigs-static.dwarfmaster.net";
  };
}
