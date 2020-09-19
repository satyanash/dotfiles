#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Add vim as default editor
export EDITOR=vim

# history control
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
	. /etc/bash_completion
    fi
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # turn on bash completion [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
	source $(brew --prefix)/etc/bash_completion
    fi
fi

if [[ $(command -v bat) ]]; then
  alias cat='bat'
fi

# ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
if [[ "$OSTYPE" == "linux-gnu" ]]; then
  alias ls='ls --color=auto'
elif [[ "$OSTYPE" == "darwin"* ]]; then
  alias ls='ls -G'
fi

# grep aliases
alias grep='grep --color'

# git aliases
alias gpr='git pull --rebase'
alias gdc='git diff --cached'
alias gdf='git diff'
alias gap='git add -p'
alias gsp='git stash -p'
alias gst='git status'
alias gcm='git commit -m'

# colorify watch
alias watch='watch --color'

# make rm safe
alias rm='rm -i'

alias pingoo="ping google.com"

if [[ "$OSTYPE" == "linux-gnu" ]]; then
  # OSX style clipboard utils on X11
  alias pbcopy='xclip -selection clipboard'
  alias pbpaste='xclip -selection clipboard -o'
fi

#alias em='emacsclient --create-frame --no-wait -e "(x-focus-frame nil)"'
alias em='emacsclient --create-frame --no-wait'
export ALTERNATE_EDITOR=""

# Load rbenv automatically by appending
# the following to ~/.bash_profile:

if [[ $(command -v rbenv) ]]; then
  eval "$(rbenv init -)"
fi

if [[ $(command -v z) ]]; then
  . /usr/local/etc/profile.d/z.sh
fi

## prompt hackery
__prompt_command() {
  local EXIT="$?"             # This needs to be first
  PS1=""
  #local boxname=$(scutil --get ComputerName)
  local boxname=$(hostname -s)

  local txtgrn='\[\e[1;32m\]' # Green
  local txtblu='\[\e[34m\]'   # Blue
  local txtwht='\[\e[1;37m\]' # White
  local txtrst='\[\e[0m\]'    # Text Reset
  local txtdim='\[\e[2m\]'    # dim

  # username@hostname
  PS1+="${txtwht}\u${txtrst}"
  PS1+="@"
  PS1+="${txtwht}${boxname}${txtrst}"

  # pwd
  PS1+=":${txtblu}\W${txtrst}"

  if [[ $EXIT -ne 0 ]]; then
    # last exit status
    PS1+=" ${txtdim}${EXIT}${txtrst}"
  fi

  # prompt
  PS1+=" $ "
}

PROMPT_COMMAND=__prompt_command

if [[ $(command -v fortune) && $(command -v cowsay) && $(command -v lolcat) ]]; then
  fortune | cowsay | lolcat
fi

function dockill() {
  docker stop $1 && docker rm $1
}

function ctags_ruby() {
  ctags -R --languages=ruby --exclude=.git
}

function ctags_python() {
  ctags -R --languages=python --exclude=.git
}

function ctags_java() {
  ctags -R --languages=python --exclude=.git
}

function ctags_c() {
  ctags -R --languages=c --exclude=.git
}

function genpass() {
  curl "https://www.random.org/strings/?num=10&len=15&digits=on&upperalpha=on&loweralpha=on&unique=on&format=plain&rnd=new"
}

if [[ -f ~/.bash_local.sh ]]; then
  source ~/.bash_local.sh
fi
