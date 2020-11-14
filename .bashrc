# tmux 256 colour alias
alias tmux="tmux -2"

# Local customized path and environment settings, etc.
if [ -f ~/.bashrc.local ]; then
    . ~/.bashrc.local
fi

if [ -f ~/.bash_profile ]; then
    . ~/.bash_profile
fi
