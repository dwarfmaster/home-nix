{ pkgs, recdata, ... }:

{
  programs.neovim = {
    enable      = true;
    withPython  = true;
    withPython3 = true;
    withRuby    = true;
    viAlias     = true;
    vimAlias    = true;

    configure = {
      customRC = builtins.readFile ./vimrc;

      packages.myVimPackage = with pkgs.vimPlugins; {
        # Loaded on launch
        start = [
            # Apparence
            base16-vim    # Base16 color schemes
            lightline-vim # Status line

            # QOL
            vim-multiple-cursors # Brings multiple cursors to vim

            # Languages
            vim-addon-nix # Nix support in vim
        ];

        # Load on demand with ':packadd $pkgname'
        opt = [ ];
      };
    };
  };
}

