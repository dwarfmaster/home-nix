{ system, tungdil, vraccas, ... }:

{
  name = "trivial";
  mate.description = "Checks that Tungdil boots";

  nodes = {
    tungdil = { ... }: { imports = tungdil; };
    vraccas = { ... }: { imports = vraccas; };
  };

  testScript = ''
    tungdil.start()
    tungdil.wait_for_unit("default.target")
    print("Tungdil successfully started")
  '';
}
