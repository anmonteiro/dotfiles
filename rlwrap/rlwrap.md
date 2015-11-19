## rlwrap installation notes:

1. Download [GNU readline](https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html) ([latest link](http://git.savannah.gnu.org/cgit/readline.git/snapshot/readline-master.tar.gz))
2. Compile & install readline from source with the usual commands (`./configure`, `make`, `make install`)
3. Download [rlwrap](http://utopia.knoware.nl/~hlub/uck/rlwrap/) ([GitHub repo](https://github.com/hanslub42/rlwrap)) [tarball](http://utopia.knoware.nl/~hlub/uck/rlwrap/rlwrap-0.42.tar.gz)
4. Run the following:
  1. `LDFLAGS=-L/usr/local/lib CPPFLAGS=-I/usr/local/include ./configure`
  2. `make`
  3. (optional) `make check` to test everything is correctly built
  4. `make install`
5. rlwrap should now be correctly installed
