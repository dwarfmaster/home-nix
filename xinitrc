#!/usr/bin/env sh

# System-wide configuration
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# DBus is not setted up properly with the startx method
if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
  eval $(dbus-launch --exit-with-session --sh-syntax)
fi
systemctl --user import-environment DISPLAY XAUTHORITY
if command -v dbus-update-activation-environment >/dev/null 2>&1; then
  dbus-update-activation-environment DISPLAY XAUTHORITY
fi

# WM-independent config
xmodmap -e "keycode 65 = space space space space" # shift+space <- 4*space
xset b off

# redshift ?

# XMonad
xmonad

