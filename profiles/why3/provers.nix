 {
  z3 = {
    defaultDriver = "z3";
    defaultEditor = "";
    config = cfg: ''
command = "${cfg.package}/bin/z3 -smt2 -T:%t sat.random_seed=42 nlsat.randomize=false smt.random_seed=42 -st %f"
command_steps = "${cfg.package}/bin/z3 -smt2 sat.random_seed=42 nlsat.randomize=false smt.random_seed=42 -st rlimit=%S %f"
driver = "${cfg.driver}"
editor = "${cfg.editor}"
in_place = false
interactive = false
name = "Z3"
shortcut = "${cfg.shortcut}"
shortcut = "z3"
version = "${cfg.package.version}"
    '';
  };

  cvc4 = {
    defaultDriver = "cvc4_17";
    defaultEditor = "";
    config = cfg: ''
command = "${cfg.package}/bin/cvc4 --stats --tlimit=%t000 --lang=smt2 %f"
command_steps = "${cfg.package}/bin/cvc4 --stats --rlimit=%S --lang=smt2 %f"
driver = "${cfg.driver}"
editor = "${cfg.editor}"
in_place = false
interactive = false
name = "CVC4"
shortcut = "${cfg.shortcut}"
shortcut = "cvc4"
version = "${cfg.package.version}"
    '';
  };

  alt-ergo = {
    defaultDriver = "alt-ergo";
    defaultEditor = "altgr-ergo";
    config = cfg: ''
command = "${cfg.package}/bin/alt-ergo -timelimit %t %f"
command_steps = "${cfg.package}/bin/alt-ergo -steps-bound %S %f"
driver = "${cfg.driver}"
editor = "${cfg.editor}"
in_place = false
interactive = false
name = "Alt-Ergo"
shortcut = "${cfg.shortcut}"
shortcut = "alt-ergo"
version = "${cfg.package.version}"
    '';
  };

  coq = {
    defaultDriver = "coq";
    defaultEditor = "coqide";
    config = cfg: ''
command = "${cfg.package}/bin/coqtop -batch -R %l/coq Why3 -l %f"
driver = "${cfg.driver}"
editor = "${cfg.editor}"
in_place = false
interactive = true
name = "Coq"
shortcut = "${cfg.shortcut}"
shortcut = "coq"
version = "${cfg.package.version}"
    '';
  };
}
