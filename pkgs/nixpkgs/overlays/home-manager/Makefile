VERSIONS=release-19.09 release-20.03

init: $(VERSIONS)

release-%: master
	cd master && git worktree add ../$@ origin/$@

.PHONY:init


