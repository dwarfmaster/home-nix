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
          rev    = "4287470cd3efb1f8656bafc000ceb9f8d9df178b";
          sha256 = "1d9i96r0kv2r8cxyrpii8xfalbni50gsamad76yrhplyk0ziwaix";
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

