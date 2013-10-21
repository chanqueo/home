#!/bin/bash

git_setup()
{
    if git --git-dir=$2/.git --work-tree=$2 status 2>&- 1>&-
    then
        git --git-dir=$2/.git --work-tree=$2 pull
    else
        git clone $1 $2
    fi
}

git_setup https://github.com/immerrr/lua-mode.git lua-mode
