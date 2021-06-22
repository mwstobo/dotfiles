# ls aliases
alias ls="ls --color --group-directories-first"
alias ll="ls -lh"
alias la="ls -a"

# sudo completion
complete -cf sudo

# history control
export HISTCONTROL=ignoreboth
export HISTFILESIZE=
export HISTSIZE=

# Editor
export EDITOR="nvim"
export DIFFPROG="nvim -d"
alias vim="nvim"

# Load the default .profile
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# Local customized path and environment settings, etc.
if [ -f ~/.bash_profile.local ]; then
    . ~/.bash_profile.local
fi
