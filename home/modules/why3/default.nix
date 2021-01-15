general@{ lib, recdata, ... }:

let
  pkgs = general.pkgs.main;
in {
  packages = with pkgs; [ why3 ];

  home.file.".why3.conf".text = ''
[main]
default_editor = "vim %f"
magic = 14
memlimit = 1000
running_provers_max = 2
timelimit = 5

[ide]
font_size = 14
error_color_bg = "darkgrey"
error_color_fg = "red"
error_color_msg_zone_bg = "darkgoldenrod"
error_color_msg_zone_fg = "white"
error_line_color = "darkgoldenrod"
goal_color = "darkgoldenrod"
premise_color = "darkgreen"

[prover]
command = "${pkgs.why3-z3}/bin/z3 -smt2 -T:%t sat.random_seed=42 nlsat.randomize=false smt.random_seed=42 -st %f"
command_steps = "${pkgs.why3-z3}/bin/z3 -smt2 sat.random_seed=42 nlsat.randomize=false smt.random_seed=42 -st rlimit=%S %f"
driver = "z3"
editor = ""
in_place = false
interactive = false
name = "Z3"
shortcut = "z3-dev"
shortcut = "z3"
version = "4.8.6"

[prover]
command = "${pkgs.coq}/bin/coqtop -batch -R %l/coq Why3 -l %f"
driver = "coq"
editor = "coqide"
in_place = false
interactive = true
name = "Coq"
shortcut = "coq-dev"
shortcut = "coq"
version = "8.11.2"

[prover]
command = "${pkgs.why3-cvc4}/bin/cvc4 --stats --tlimit=%t000 --lang=smt2 %f"
command_steps = "${pkgs.why3-cvc4}/bin/cvc4 --stats --rlimit=%S --lang=smt2 %f"
driver = "cvc4_17"
editor = ""
in_place = false
interactive = false
name = "CVC4"
shortcut = "cvc4-why3"
shortcut = "cvc4"
version = "1.7"

[prover]
command = "${pkgs.why3-alt-ergo}/bin/alt-ergo -timelimit %t %f"
command_steps = "${pkgs.why3-alt-ergo}/bin/alt-ergo -steps-bound %S %f"
driver = "alt_ergo"
editor = "altgr-ergo"
in_place = false
interactive = false
name = "Alt-Ergo"
shortcut = "altergo-dev"
shortcut = "alt-ergo"
version = "2.3.1"
  '';
}
