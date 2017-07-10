# dotfiles

## Symlinking to dotfiles

From [Git, dotfiles & Hardlinks](https://codingkilledthecat.wordpress.com/2012/08/08/git-dotfiles-and-hardlinks/):

symlinking files inside the repository out of it, into your home directory:

```shell
~ $ rm ~/.emacs.d
~ $ cd ~
~ $ ln -s dotfiles/.emacs.d .
```

## Installing Emacs

### Standard version

``` shell
$ brew install emacs --HEAD --with-cocoa --with-modules --with-ctags --with-dbus --with-gnutls --with-rsvg --imagemagick
```

### Railwaycat Emacs port for macOS

```shell
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac
```
