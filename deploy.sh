#!/bin/sh

rsync -a --exclude=elpa --delete .emacs.d/ ~/.emacs.d/

cp -a .inputrc ~
cp -a .bash_custom ~
cp -a .git-completion.sh ~
cp -a .xbindkeysrc ~

#cp ipythonrc-csb ~/.ipython/
#cp ipy-virtualenv.py ~/.ipython/

mkdir -p ~/bin
cp bin/* ~/bin

if   `grep -q bash_custom  ~/.bashrc`
then
    `true`
else
    echo "" >> ~/.bashrc
    echo "source ~/.bash_custom" >> ~/.bashrc
fi
