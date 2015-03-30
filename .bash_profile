[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default .profile

# rvm config
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
# end rvm config

# pyenv config
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
# end pyenv config

# virtualenv config
eval "$(pyenv virtualenv-init -)"
# end virtualenv config

# tmuxifier config
export PATH="$HOME/.tmuxifier/bin:$PATH"
eval "$(tmuxifier init -)"
# end tmuxifier config
