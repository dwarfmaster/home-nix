
init:
	git submodule init
	make -C pkgs/nixpkgs init

.PHONY: init


