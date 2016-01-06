# ls aliases
alias ls="ls --color"
alias ll="ls -lh"
alias la="ls -a"

# tmux 256 colour alias
alias tmux="tmux -2"

# sudo completion
complete -cf sudo

# ignore duplicates
export HISTCONTROL=ignoredups

# PS1
export PS1="\u@\h \w \\$ \[$(tput sgr0)\]"

# Local customized path and environment settings, etc.
if [ -f ~/.bashrc.local ]; then
    . ~/.bashrc.local
fi
