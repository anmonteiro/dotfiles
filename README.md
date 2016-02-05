# dotfiles

From [Git, dotfiles & Hardlinks](https://codingkilledthecat.wordpress.com/2012/08/08/git-dotfiles-and-hardlinks/):

symlinking files inside the repository out of it, into your home directory:

```shell
~/dotfiles $ rm ~/.emacs.d
~/dotfiles $ cd ~
~ $ ln -s dotfiles/.emacs.d .
```
