{ system, ... }:

{
  name = "trivial";
  meta = {
    description = "A trivial test that boots an empty VM";
  };

  nodes = {
    vm = { ... }: { };
  };

  testScript = ''
    vm.start()
    vm.wait_for_unit("default.target")
    print("Empty VM successfully started")
  '';
}
