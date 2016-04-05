#!/bin/sh

cp .emacs ~
mkdir -p ~/.emacs.d/
cp -ra .emacs.d/* ~/.emacs.d/

cp .inputrc ~
cp .bash_custom ~
cp .git-completion.sh ~

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
