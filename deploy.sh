#!/bin/sh

#TODO: some kind of archiving

# For now all we do is copy over.
cp .emacs ~
cp -r emacs ~/

cp .inputrc ~
cp .bash_custom ~
cp .git-completion.sh ~

cp ipythonrc-csb ~/.ipython/
cp ipy-virtualenv.py ~/.ipython/

mkdir -p ~/bin
cp bin/* ~/bin

if   `grep -q bash_custom  ~/.bashrc`
then
    `true`
else
    echo "" >> ~/.bashrc
    echo "source ~/.bash_custom" >> ~/.bashrc
fi