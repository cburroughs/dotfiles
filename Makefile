SHELL=/bin/bash

most: emacs bash git bin-utils x11
.PHONY: emacs bash git bin-utils x11 first-run-only

emacs:
	@echo $@
	rsync -va --exclude=elpa --delete .emacs.d/ ~/.emacs.d/

bash:
	@echo $@
	cp -a .bash_custom ~
	@if  ! `grep -q bash_custom  ~/.bashrc`; then \
	    echo "" >> ~/.bashrc; \
	    echo "source ~/.bash_custom" >> ~/.bashrc; \
	fi

git:
	@echo $@
	mkdir -p ~/.config
	cp -a git-prompt.sh ~/.config/git-prompt.sh

bin-utils:
	@echo $@
	mkdir -p ~/bin
	cp -a bin/* ~/bin

x11:
	@echo $@
	cp -a .xprofile ~
	cp -a .xbindkeysrc ~

# Pseudo tied to hardware or otherwise specific per workstation
hw:
	cp -a .xmodmaprc ~
