{
  pkgs,
  config,
  ...
}: let
  vim-asciidoctor = pkgs.vimUtils.buildVimPlugin {
    name = "vim-asciidoctor";
    src = pkgs.fetchFromGitHub {
      owner = "habamax";
      repo = "vim-asciidoctor";
      rev = "f553311b5db03440eb8d7035434d0405e4a2c559";
      sha256 = "05sbipvsrv4zbgg6k0glr0syj9q5zipp6wylhffln6awq8r7n3j9";
    };
  };
in {
  programs.nixvim = {
    extraPlugins = [vim-asciidoctor];
    globals = {
      asciidoctor_executable = "${pkgs.asciidoctor}/bin/asciidoctor";
      asciidoctor_folding = 1;
      asciidoctor_fold_options = 0;
      asciidoctor_syntax_conceal = 1;
      asciidoctor_syntax_indented = 1;
      asciidoctor_fenced_languages = ["c" "cpp" "haskell" "rust" "ruby" "prolog"];
    };
    extraConfigLua = builtins.readFile ./utils.lua;
  };

  home.packages = [pkgs.asciidoctor];
}
