let
  fzf-bibtex-fetch = {
      type   = "git";
      url    = "https://github.com/msprev/fzf-bibtex";
      rev    = "72ce2f75b236588602985adb79bb72ddb5b3cba3";
      sha256 = "14k0dyq3pppwa4dqfn95dn35mxz7frg71a259xirs6qi20mchcjj";
    };
in [
  {
    goPackagePath = "github.com/msprev/fzf-bibtex/bibtex";
    fetch = fzf-bibtex-fetch;
  }

  {
    goPackagePath = "github.com/msprev/fzf-bibtex/cache";
    fetch = fzf-bibtex-fetch;
  }

  {
    goPackagePath = "github.com/msprev/fzf-bibtex/format";
    fetch = fzf-bibtex-fetch;
  }

  {
    goPackagePath = "github.com/msprev/fzf-bibtex/startup";
    fetch = fzf-bibtex-fetch;
  }
]
