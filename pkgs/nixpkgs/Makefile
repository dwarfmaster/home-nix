VERSIONS=nixos-19.03 nixos-19.09 nixos-20.03 nixos-unstable

init: $(VERSIONS)

nixos-%: master
	cd master && git worktree add ../$@ origin/$@

.PHONY:init


