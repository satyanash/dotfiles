#!/bin/bash

# all OSX specific stuff goes in .bash_profile

# Needed for OSX zsh prompt
export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -f /opt/homebrew/bin/brew ]; then
    # auto-detect if homebrew is installed to /opt/homebrew
    eval $(/opt/homebrew/bin/brew shellenv)
    export CPPFLAGS="-I$(brew --prefix)/include${CPPFLAGS+ ${CPPFLAGS}}"
    export LDFLAGS="-L$(brew --prefix)/lib -Wl,-rpath,$(brew --prefix)/lib${LDFLAGS+ ${LDFLAGS}}"
    export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
fi

if [ -f ~/.bashrc ]; then
    # all generic stuff goes in .bashrc
    . ~/.bashrc
fi
