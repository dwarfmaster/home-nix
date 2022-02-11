{ system, tungdil, vraccas, ... }:

{
  name = "boot-vraccas";
  mate.description = "Checks that Vraccas boots";

  nodes = {
    vraccas = { ... }: { imports = vraccas; };
  };

  testScript = ''
    vraccas.start()
    vraccas.wait_for_unit("default.target")
    print("Vraccas successfully started")
  '';
}
