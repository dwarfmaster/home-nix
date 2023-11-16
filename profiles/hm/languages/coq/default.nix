{
  config,
  lib,
  pkgs,
  ...
}: let
  packages = pkgs.coqPackages_8_15;
in {
  programs.coq = {
    enable = true;
    inherit packages;
    libraries = [
    ];
  };
  programs.doom-emacs.config.initModules.lang = ["coq"];

  # Neovim
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.Coqtail];
    globals = {
      coqtail_nomap = 1;
      coqtail_coq_path = "${config.programs.coq.package}/bin";
      coqtail_noindent_comment = 1;
      coqtail_auto_set_proof_diffs = "off";
    };
    files."ftplugin/coq.lua" = {
      keymaps = let
        mk = key: action: desc: {
          mode = "n";
          inherit key;
          action = ":${action}<cr>";
          options.desc = desc;
        };
      in [
        (mk "<leader> ps" "CoqStart" "Start Coq")
        (mk "<leader> pq" "CoqStop" "Stop Coq")
        (mk "<leader> pi" "CoqInterrupt" "Interrupt Coq")
        (mk "<leader> j" "CoqNext" "Check next sentence")
        (mk "<leader> k" "CoqUndo" "Undo last sentense")
        (mk "<leader> ." "CoqToLine" "Check to current line")
        (mk "<leader> gg" "CoqToTop" "Rewind to the beggining of file")
        (mk "<leader> gc" "CoqJumpToEnd" "Move cursor the end of checked")
        (mk "<leader> ge" "CoqJumpToError" "Move cursor to error")
        (mk "<leader> R" "CoqRestorePanels" "Restore panels")
        (mk "]g" "CoqGotoGoalNext" "Scroll to next goal")
        (mk "]G" "CoqGotoGoalNext!" "Scroll to end of next goal")
        (mk "[g" "CoqGotoGoalPrev" "Scroll to prev goal")
        (mk "[G" "CoqGotoGoalPrev!" "Scroll to end of prev goal")
      ];
    };
  };

  # When installing coq through opam, it needs gmp and zlib
  home.packages = [pkgs.gmp pkgs.zlib];
  pkgconfig.enable = true;
  pkgconfig.path = ["${pkgs.gmp.dev}/lib/pkgconfig" "${pkgs.zlib.dev}/lib/pkgconfig"];
}
