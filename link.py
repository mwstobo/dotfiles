import os
import os.path

manifest = open('Manifest')
dotfile_mappings = manifest.read().splitlines()

if os.environ.get("XDG_CONFIG_HOME") is None:
    os.environ["XDG_CONFIG_HOME"] = os.path.expandvars("$HOME/.config")

for mapping in dotfile_mappings:
    source, target = mapping.split("~>")
    source_file = "{}/{}".format(os.getcwd(), source.strip())
    target_file = os.path.expandvars(target.strip())
    if os.path.isfile(target_file):
        os.remove(target_file)
    os.symlink(source_file, target_file)
