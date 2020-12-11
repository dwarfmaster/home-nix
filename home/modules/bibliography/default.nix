general:

let
  pkgs = general.pkgs.main;
  bibtexdb = "/home/luc/skills/db.bib";
in {
  packages = with pkgs; [
    bibtool  # CLI util to manage a bibtex database
             # Documentation saved at ~/workflow/config/bibtool.pdf
  ];

  globalVariables = {
    BIBTEXDB            = "${bibtexdb}";
    FZF_BIBTEX_CACHEDIR = "\$XDG_CACHE_HOME/fzf-bibtex";
    FZF_BIBTEX_SOURCES  = "${bibtexdb}";
  };
}
