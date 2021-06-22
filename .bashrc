# tmux 256 colour alias
alias tmux="tmux -2"

# PS1
export PS1="\u@\h \w \\n\$ \[$(tput sgr0)\]"

# Local customized path and environment settings, etc.
if [ -f ~/.bashrc.local ]; then
    . ~/.bashrc.local
fi

if [ -f ~/.bash_profile ]; then
    . ~/.bash_profile
fi
