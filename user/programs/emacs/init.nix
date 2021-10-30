{
  app = [
    "calendar"
    { mod = "rss"; args = [ "org" ]; }
  ];

  checkers = [
    { mod = "syntax"; args = [ "childframe" ]; }
    { mod = "spell"; args = [ "flyspell" "everywhere" "hunspell" ]; }
    "grammar"
  ];

  completion = [
    { mod = "company"; args = [ "tng" ]; }
    { mod = "helm"; args = [ "fuzzy" ]; }
  ];

  config = [
    { mod = "default"; args = [ "bindings" "smartparens" ]; }
  ];

  editor = [
    { mod = "evil"; args = [ "everywhere" ]; }
    "file-templates"
    "fold"
    "lispy"
    "snippets"
  ];

  emacs = [
    { mod = "dired"; args = [ "icons" "ranger" ]; }
    { mod = "ibuffer"; args = [ "icons" ]; }
    { mod = "undo"; args = [ "tree" ]; }
    "vc"
  ];

  lang = [
    { mod = "agda"; args = [ "local" ]; }
    { mod = "cc"; args = [ "lsp" ]; }
    "coq"
    "lean"
    "emacs-lisp"
    { mod = "haskell"; args = [ "dante" ]; }
    "idris"
    { mod = "latex"; args = [ "latexmk" "cdlatex" "lsp" ]; }
    "ledger"
    "nix"
    # "ocaml"
    { mod = "org"; args = [ "roam" "pretty" ]; }
    "rust"
    "sh"
    "web"
    { mod = "julia"; args = [ "lsp" ]; }
  ];

  term = [
    # "eshell"
  ];

  tools = [
    "biblio"
    "direnv"
    { mod = "debugger"; args = [ "lsp" ]; }
    { mod = "eval"; args = [ "overlay" ]; }
    { mod = "lookup"; args = [ "dictionary" "docsets" ]; }
    { mod = "lsp"; args = [ "peek" "eglot" ]; }
    "magit"
    "make"
    { mod = "pass"; args = [ "auth" ]; }
    "rgb"
  ];

  ui = [
    "doom"
    "doom-dashboard"
    "doom-quit"
    { mod = "emoji"; args = [ "github" "unicode" ]; }
    "hl-todo"
    "hydra"
    { mod = "ligatures"; args = [ "extra" "fira" ]; }
    "modeline"
    "nav-flash"
    "neotree"
    "ophints"
    { mod = "popup"; args = [ "all" "defaults" ]; }
    "window-select"
    "workspace"
  ];
}
