# ls aliases
alias ls="ls --color --group-directories-first"
alias ll="ls -lh"
alias la="ls -a"

# tmux 256 colour alias
alias tmux="tmux -2"

# sudo completion
complete -cf sudo

# history control
export HISTCONTROL=erasedups
export HISTFILESIZE=
export HISTSIZE=

# PS1
export PS1="\u@\h \w \\n\$ \[$(tput sgr0)\]"

# Editor
export EDITOR="vim"

# Local customized path and environment settings, etc.
if [ -f ~/.bashrc.local ]; then
    . ~/.bashrc.local
fi
