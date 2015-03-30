cd ~/.dotfiles
dotfiles=`ls $PWD/.??*`
for dotfile in $dotfiles; do
    ln -fs $dotfile ~
done
