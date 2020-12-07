VERSIONS=19.09 20.03 20.09

init: $(addprefix nixos-, $(VERSIONS))

nixos-%: master
	test -e $@ || { cd master; git worktree add ../$@ origin/$@; }

update-all: update-master $(add-prefix update-, $(VERSIONS))

update-master:
	cd master && git fetch origin

update-%: nixos-% 
	cd $< && git merge --ff-only origin/$<

.PHONY:init update-all update-master


