#!/bin/bash

git_setup()
{
    if git --git-dir=$(pwd)/$1/.git --work-tree=$(pwd)/$1 status 2>&- 1>&-
    then
        git --git-dir=$(pwd)/$1/.git --work-tree=$(pwd)/$1 pull
    else
        git clone $2 $(pwd)/$1
    fi
}

git_setup lua-mode https://github.com/immerrr/lua-mode.git
git_setup tabbar https://github.com/dholm/tabbar.git
