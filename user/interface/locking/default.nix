{ config, lib, ... }:

# TODO setting locking by session on loginctl event
let
  inherit (config.pkgsets) pkgs;

  physlock = "${pkgs.physlock}/bin/physlock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";
  vlock = "${pkgs.vlock}/bin/vlock";
  setxkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";

  # Make it a program somewhere else
  unsetxkbmap = pkgs.writeScript "unsetxkbmap"
    ''
    #!${config.programs.nushell.package}/bin/nu

    let toremove = $nu.env.XKBOPTION

    # Get all options
    let options = (setxkbmap -query | lines | split column ': ' | str trim | pivot -ri | get options | split row ',')

    # Unset all options
    ${setxkbmap} -option

    # Reset all other options
    echo $options | where $it !~ $toremove | each { ${setxkbmap} -option $it }
    '';

  locker = pkgs.writeShellScript "lock-xsession"
    ''
      # Disable switching to VTs
      ${setxkbmap} -option srvrkeys:none
      # Lock X11 screen
      ${i3lock} -n
      # Re-enable switching to VTs
      XKBOPTION=srvrkeys:none ${unsetxkbmap}
    '';
in {
  home.packages = [ pkgs.physlock pkgs.vlock pkgs.i3lock ];
  programs.zsh.shellAliases = {
    xlock = "${locker}";
    unsetx = "${unsetxkbmap}";
  };
  programs.bash.shellAliases = {
    xlock = "${locker}";
  };
}
