SHELL=/bin/bash

most: emacs bash git bin-utils x11
.PHONY: emacs bash git bin-utils x11 first-run-only xfce4-keyboard-shortcuts

emacs:
	@echo $@
	rsync -va --exclude=elpa --exclude=straight --exclude=eln-cache --delete .emacs.d/ ~/.emacs.d/

straight-slurp-lock:
	@echo $@
	cp -a ~/.emacs.d/straight.lockfile.el .emacs.d/

bash:
	@echo $@
	cp -a .inputrc ~
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

xfce4-keyboard-shortcuts:
	mkdir -p ~/.config/xfce4/xfconf/xfce-perchannel-xml
	cp -a xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml ~/.config/xfce4/xfconf/xfce-perchannel-xml/

xfce4-applications:
	mkdir -p ~/.local/share/applications/
	cp -a desktop-applications/* ~/.local/share/applications/
