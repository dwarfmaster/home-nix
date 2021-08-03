{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    stdlib = ''
direnv_layout_dir() {
  pwd_hash=$(echo -n $PWD | ${pkgs.perl}/bin/shasum | cut -d ' ' -f 1)
  echo "$XDG_CACHE_HOME/direnv/layouts/$pwd_hash"
}

use_flake() {
  watch_file flake.nix
  watch_file flake.lock
  eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
}
'';
  };
}

