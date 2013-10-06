#!/bin/bash

ln -fs $(pwd) ~/.emacs.d

cd packages; ./setup.sh; cd ..
cd lisp; ./setup.sh; cd ..
