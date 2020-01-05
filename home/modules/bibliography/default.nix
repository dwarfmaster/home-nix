general:

let
  pkgs = general.pkgs.main;
in {
  packages = with pkgs; [
    bibtool  # CLI util to manage a bibtex database
             # Documentation saved at ~/workflow/config/bibtool.pdf
  ];

  shellVariables = {
    BIBTEXDB = "/home/luc/skills/db.bib";
  };
}

