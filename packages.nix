pkgs:

let base = with pkgs; [
  openssh
  graphviz
  ctags
  file
  patchelf
  mr
  gnupg
  gnupg1
  pinentry_ncurses
  unrar
  p7zip
  lm_sensors
  pciutils
  cryptsetup
  beep
  unzip
  libmtp
  bc
  ethtool
  postgresql
  lsof
  bridge-utils
  mtpfs
  telnet
  alsaUtils
  binutils
]; in

let shell = with pkgs; [
  zsh dash bash fish
  tmux
  abduco
  dvtm
  (vim_configurable.override {
    config.vim = {
      ruby = true;
      perl = true;
      lua  = true;
    };
    inherit ruby perl lua;
  })
  remind
  wyrd
  pass
  most
  python35Packages.youtube-dl
  stress
  librsvg
  sl
  cowsay
  bvi
  jq
  valgrind
  newsboat
  cpufrequtils
  tree
  gnum4
  ghostscript
  links
  cmatrix
  sc-im
  figlet
  pandoc
]; in

let dev = with pkgs; [
  (haskellPackages.ghcWithHoogle (hpkgs: with hpkgs; [ xmobar xmonad xmonad-contrib xmonad-extras ]))
  perl
  python2
  python3
  ruby
  cabal-install
  gitAndTools.git-annex
  pijul
  gcc
  gdb
  gnumake
  pkgconfig
  ddd
  cloc
]; in

let desktop = with pkgs; [
  haskellPackages.xmonad
  haskellPackages.xmobar
  dmenu
  st
  redshift
  xorg.xev
  xorg.xprop
  glxinfo
  numlockx
  imlibsetroot
  lighthouse
  gparted
  woeusb
  xclip
  calibre
  gnumeric
  abiword
  wireshark
  i3lock
  julia
  solvespace
  freecad
]; in
  
let multimedia = with pkgs; [
  audacity
  blender
  leocad
  gimp
  mpv
  okular
  zathura
  sxiv
  inkscape
  imagemagick
  qpdf
  fanficfare
]; in

base ++ shell ++ dev ++ desktop ++ multimedia

