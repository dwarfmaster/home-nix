VERSIONS=release-19.09

init: $(VERSIONS)

release-%: master
	cd master && git worktree add ../$@ origin/$@

.PHONY:init


