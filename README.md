# dotfiles

## Symlinking to dotfiles

From [Git, dotfiles & Hardlinks](https://codingkilledthecat.wordpress.com/2012/08/08/git-dotfiles-and-hardlinks/):

symlinking files inside the repository out of it, into your home directory:

```shell
ln -sfn $PWD/nvim ~/.config/nvim
```

```shell
~ $ rm ~/.emacs.d
~ $ cd ~
~ $ ln -s dotfiles/.emacs.d .
```
