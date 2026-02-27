# config files
This is where I version control my configs for various OS setups and tools.

I use [stow](https://www.gnu.org/software/stow/) to create symlinks to their proper directories.

I clone this repository alongside my other projects: `Develop/config`. The changes I make here are reflected through the symlinks.


## common
### neovim
```
cd ~/Develop/config/common
stow -v nvim -t ~/.config/nvim
```
### emacs
```
cd ~/Develop/config/common/
stow -v emacs -t ~/.emacs.d
```

## fedora
```
stow -v bashrc sway waybar udev gtk -t ~
```
