[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default .profile

FILE=~/.bash_profile_local && test -f $FILE && source $FILE
