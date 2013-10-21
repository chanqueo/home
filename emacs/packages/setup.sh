#!/bin/bash

git_setup()
{
    local repo=$1
    local dir=$2

    if [ -d $dir ]
    then
        echo Update $repo
        cd $dir; git pull; cd ..
    else
        echo Clone $repo
        git clone $repo $dir
    fi
}

git_setup https://github.com/immerrr/lua-mode.git lua-mode
