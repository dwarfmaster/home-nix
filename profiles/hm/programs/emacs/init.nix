{
  checkers = [
    {
      mod = "syntax";
      args = ["childframe"];
    }
    {
      mod = "spell";
      args = ["flyspell" "everywhere" "hunspell"];
    }
    "grammar"
  ];

  completion = [
    {
      mod = "company";
      args = ["tng"];
    }
    {
      mod = "helm";
      args = ["fuzzy"];
    }
  ];

  config = [
    {
      mod = "default";
      args = ["bindings" "smartparens"];
    }
  ];

  editor = [
    {
      mod = "evil";
      args = ["everywhere"];
    }
    "file-templates"
    "fold"
    "lispy"
    "snippets"
  ];

  emacs = [
    {
      mod = "dired";
      args = ["icons" "ranger"];
    }
    {
      mod = "ibuffer";
      args = ["icons"];
    }
    {
      mod = "undo";
      args = ["tree"];
    }
    "imenu"
  ];

  lang = [
    "emacs-lisp"
    "sh"

    # Languages that may have their own modules if I use them enough
    # "ocaml"
    "rust"
    "web"
  ];

  # term = [
  #   "eshell"
  # ];

  ui = [
    "doom"
    "doom-dashboard"
    "doom-quit"
    {
      mod = "emoji";
      args = ["github" "unicode"];
    }
    "hl-todo"
    "hydra"
    "modeline"
    "nav-flash"
    "minimap"
    "treemacs"
  ];
}
