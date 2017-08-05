# Load the default .profile
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# Local customized path and environment settings, etc.
if [ -f ~/.bash_profile.local ]; then
    . ~/.bash_profile.local
fi

# Load bashrc
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
