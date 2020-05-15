general:

let

  pkgs = general.pkgs.main;

  f = { mkDerivation, base, containers, dhall, directory, filepath
      , optparse-applicative, prettyprinter, relude, stdenv
      , template-haskell, text
      }:
      mkDerivation (with pkgs; {
        pname = "project-manager";
        version = "0.0.1";
        src = fetchFromGitHub {
          owner  = "dwarfmaster";
          repo   = "project-manager";
          rev    = "edd3b72911a45cf11a2d6391e3879bb7e73dcaad";
          sha256 = "0cszzidjniw04cn8495jmy759741xl94gjvp7wpppnc6xnz5hqn9";
        };
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers dhall directory filepath optparse-applicative
          prettyprinter relude template-haskell text
        ];
        description = "Helper software to manage my projects descriptions";
        license = stdenv.lib.licenses.mit;
      });

in pkgs.haskellPackages.callPackage f {}

