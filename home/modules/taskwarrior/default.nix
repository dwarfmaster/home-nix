general:

let
  pkgs = general.pkgs.main;
in {
  programs.taskwarrior = {
    enable = true;
    colorTheme = ./color.theme;
    config = {
      default.command = "ready";
      report.ready = {
        filter = "+READY project.not:Irl.improvment.bugs";
      };
    };
  };
}

