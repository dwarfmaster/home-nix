
desktops=("l4" "l3" "l2" "l1" "l0" "r0" "r1" "r2" "r3" "r4")
sep="^"

get_current() {
  bspc query -D --names -d | cut -d$sep -f1
}

get_desktop() {
  bspc query -D --names -d | cut -d$sep -f2
}

focus_desk() {
  # TODO better handle monitors
  desk=$1
  bspc desktop "$desk" -f
}

all_workspaces() {
  bspc query -D --names | cut -d$sep -f1 | sort -u
}

has_workspace() {
  all_workspaces | grep -x "$1" >/dev/null 1>&1
}

remove-workspace() {
  # TODO get list of windows that will be closed and ask for confirmation
  workspace=$1

  if ! has_workspace "$workspace"; then
    return 1
  fi

  current=$(get_current)
  if [ "$current" = "$workspace" ]; then
    desk=$(all_workspaces | grep -v -x "$current" | head -n 1)
    echo "$desk"
    focus_desk "$desk^$(get_desktop)"
  fi
  windows=0
  for desktop in ${desktops[@]}; do
    windows=$(($windows + $(bspc query -N -d "$workspace$sep$desktop" -n .leaf | wc -l)))
  done
  if [ "$windows" -gt 0 ]; then
    ans=$(echo -e "Continue\nAbort" | rofi -dmenu -p "You will close $windows windows")
    if [ "$ans" = "Abort" ]; then
      exit 1
    fi
  fi
  for desktop in ${desktops[@]}; do
    # Close windows in desktop
    bspc node "@$workspace$sep$desktop:/" -c
    # Delete desktop
    bspc desktop "$workspace$sep$desktop" -r
  done
}

create-workspace() {
  for desktop in ${desktops[@]}; do
    bspc monitor -a "$1$sep$desktop"
  done
}


# TODO better monitor support !
case $1 in
  "focus")
    focus_desk "$(get_current)$sep$2"
    ;;
  "send-to")
    DESK="$(get_current)$sep$2"
    bspc node -d "$DESK"
    ;;
  "workspace")
    get_current
    ;;
  "desktop")
    get_desktop
    ;;
  "workspaces")
    all_workspaces
    ;;
  "focus-workspace")
    focus_desk "$2$sep$(get_desktop)"
    ;;
  "focus-select")
    desk="$(get_desktop)"
    if [ "$#" -eq 2 ]; then
      desk="$2"
    fi
    focus_desk "$(all_workspaces | rofi -dmenu)$sep$desk"
    ;;
  "send-to-workspace")
    DESK="$2$sep$(get_desktop)"
    bspc node -d "$DESK"
    ;;
  "send-to-select")
    desk="$(get_desktop)"
    if [ "$#" -eq 2 ]; then
      desk="$2"
    fi
    bspc node -d "$(all_workspaces | rofi -dmenu)$sep$desk"
    ;;
  "create-workspace")
    create-workspace $2
    ;;
  "create-select")
    workspace="$(all_workspaces | rofi -dmenu)"
    if [ $? -eq 0 ]; then
      create-workspace "$workspace"
    fi
    ;;
  "remove-workspace")
    remove-workspace $2
    ;;
  "remove-select")
    workspace="$(all_workspaces | rofi -dmenu)"
    if [ $? -eq 0 ]; then
      remove-workspace "$workspace"
    fi
    ;;
  *)
    echo "Unknown command $1"
    ;;
esac

