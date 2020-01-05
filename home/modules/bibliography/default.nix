general:

let
  pkgs = general.pkgs.main;
  mpkgs = import ./packages.nix general;
  bibtexdb = "/home/luc/skills/db.bib";
in {
  packages = with pkgs; [
    bibtool  # CLI util to manage a bibtex database
             # Documentation saved at ~/workflow/config/bibtool.pdf
    mpkgs.fzf-bibtex # CLI tool to query bibtex database using fzf
  ];

  globalVariables = {
    BIBTEXDB            = "${bibtexdb}";
    FZF_BIBTEX_CACHEDIR = "\$XDG_CACHE_HOME/fzf-bibtex";
    FZF_BIBTEX_SOURCES  = "${bibtexdb}";
  };
  # TODO $FZF_BIBTEX_CACHEDIR must exist
}

