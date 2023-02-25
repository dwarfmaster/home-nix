{ lib, buildDotnetModule, fetchFromGitHub, dotnet-sdk, dotnet-runtime
, glibc, zlib, libX11, libICE, libSM, fontconfig, gtk3 }:

buildDotnetModule {
  pname = "fvim";
  version = "0.3.536";

  src = fetchFromGitHub {
    owner = "yatli";
    repo = "fvim";
    rev = "v0.3.536+gb836b56";
    sha256 = "1mgi3n72k7m9x6wfyh8jmadwi4zznk1l67n09ggnsmpx05ydfr68";
  };
  patches = [ ./fsproj.patch ];

  projectFile = "fvim.fsproj";
  nugetDeps = ./deps.nix;
  packNupkg = false;

  dotnet-sdk = dotnet-sdk;
  dotnet-runtime = dotnet-runtime;

  runtimeDeps = [
    glibc
    zlib
    libX11
    libICE
    libSM
    fontconfig
    gtk3
  ];

  meta = {
    description = "Cross platform Neovim front-end UI, built with F# + Avalonia";
    homepage = "https://github.com/yatli/fvim";
    license = lib.licenses.mit;
    maintainters = [ lib.maintainters.dwarfmaster ];
    mainProgram = "FVim";
  };
}
