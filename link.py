import os
import os.path

manifest = open('Manifest')
dotfiles = manifest.read().splitlines()

for dotfile in dotfiles:
    source_file = '{}/{}'.format(os.getcwd(), dotfile)
    target_file = '{}/{}'.format(os.path.expanduser('~'), dotfile)
    os.remove(target_file)
    os.symlink(source_file, target_file)
