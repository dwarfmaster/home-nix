{
  description = "{{cookiecutter.project_description}}";

  inputs = {
    nixpkgs = {
      url = "nixpkgs/{{cookiecutter.nixpkgs_version}}";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      {{cookiecutter.project_name}} = (pkgs.stdenv.mkDerivation {
          pname = "{{cookiecutter.project_name}}";
          version = "3.3.1";
          src = ./.;
          nativeBuildInputs = builtins.attrValues {
            inherit (pkgs)
              gcc
              cmake
              gnumake
              gtest
            ;
          };
          # TODO install phase for static, shared and header-only
          installPhase = ''
{% if cookiecutter.binary_type == "exe" -%}
            mkdir -p $out/bin
            cp bin/Release/{{cookiecutter.project_name}} $out/bin
{%- endif %}
          '';
        }
      );
    in rec {
      # TODO add tests
      defaultApp = flake-utils.lib.mkApp {
        drv = defaultPackage;
      };
      defaultPackage = {{cookiecutter.project_name}};
      devShell = pkgs.mkShell {
        inputsFrom = [
          {{cookiecutter.project_name}}
        ];
      };
    }
  );
}

