#!/bin/bash

# Needed for OSX zsh prompt
export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -f /opt/homebrew/bin/brew ]; then
	eval $(/opt/homebrew/bin/brew shellenv)
fi

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi
