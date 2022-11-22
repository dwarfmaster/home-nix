{
  config,
  lib,
  pkgs,
  ...
}: {
  # Conflicts with pipewire
  sound.enable = false;
  # TODO find precisely what it does
  security.rtkit.enable = true;
  # Enable and setup PipeWire
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
