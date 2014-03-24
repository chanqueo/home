#!/bin/bash

ln -fs $(pwd)/.bashrc ~/.bashrc
ln -fs $(pwd)/.gitconfig ~/.gitconfig
ln -fs $(pwd)/.screenrc ~/.screenrc

if [ -n "$OS" ] && [ "$OS" = "Windows_NT" ]
then
    ln -fs $(pwd)/.XWinrc ~/.XWinrc
    ln -fs $(pwd)/.startxwinrc ~/.startxwinrc
fi

cd emacs; ./setup.sh; cd ..
cd fonts; ./setup.sh; cd ..
