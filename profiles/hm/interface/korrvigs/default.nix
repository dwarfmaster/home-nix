{ config, pkgs, lib, ... }:

let
  cfg = config.services.korrvigs;
  korrvigs-builder = pkgs.writeShellScriptBin "korrvigs-builder" ''
    out=$1
    if [[ -d $out ]]; then
      rm -rf $out
    fi
    mkdir -p $out
    mkdir $out/modules
    
    sed '/user:file_search_path(korrvigs, "\/nix\/store\/.*-korrvigs-modules")\./d' \
      ${cfg.configFile} > $out/config.pl
    cat << EOF >> $out/config.pl
    user:file_search_path(korrvigs, "$(realpath $out)/modules").
    EOF
    
    ${lib.concatStringsSep "\n"
        (lib.mapAttrsToList (name: v: "install -m666 ${v} $out/modules/${name}.pl") cfg.extraModules)}

    cat << EOF > $out/test.pl
    :- [config].
    :- ['${cfg.package}/lib/korrvigs'].
    :- setup.
    EOF

    cat << EOF > $out/flake.nix
    {
      outputs = {self, nixpkgs}: let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
      in {
        devShells."x86_64-linux".default = pkgs.mkShell {
          packages = [ pkgs.swiProlog ];
        };
      };
    }
    EOF

    cat << EOF > $out/.envrc
    use flake
    EOF

    systemctl --user stop korrvigs
    cd $out
    direnv allow
  '';

  korrvigs-to-nix = pkgs.writeShellScriptBin "korrvigs-to-nix" ''
    korr=$(realpath $1)
    repo=$(realpath $2)

    for f in $(ls $korr/modules); do
      other=$(fd "^$f$" $repo | head -n 1)
      if [[ -z "$other" ]]; then
        echo "No source found for module $f"
      else
        echo "$f => $other"
        cp $korr/modules/$f $other
      fi
    done
  '';
in {
  services.korrvigs = {
    constants.term = "${config.programs.kitty.package}/bin/kitty";
    extraModules.popup = ./popup.pl;
  };
  home.packages = [ korrvigs-builder korrvigs-to-nix ];
  xsession.windowManager.bspwm.rules = {
    "popup" = {
      center = true;
      focus = true;
      follow = true;
      layer = "above";
      state = "floating";
      rectangle = "1536x864+0+0";
    };
  };
}
