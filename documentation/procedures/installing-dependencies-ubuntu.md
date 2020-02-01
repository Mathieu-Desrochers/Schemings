Installing dependencies on Ubuntu
---------------------------------
Run the following commands.

    $ sudo apt install chicken-bin
    $ sudo apt install exuberant-ctags
    $ sudo apt install gcc
    $ sudo apt install libb64-dev
    $ sudo apt install libc6-dev
    $ sudo apt install libconfig-dev
    $ sudo apt install libcurl4-openssl-dev
    $ sudo apt install libetpan-dev
    $ sudo apt install libfcgi-dev
    $ sudo apt install libicu-dev
    $ sudo apt install libjansson-dev
    $ sudo apt install libmagickwand-dev
    $ sudo apt install libpcre3-dev
    $ sudo apt install libsodium-dev
    $ sudo apt install libsqlite3-dev
    $ sudo apt install libczmq-dev
    $ sudo apt install make
    $ sudo apt install sqlite3
    $ sudo apt install texlive-full

    $ wget http://www2.informatik.uni-freiburg.de/~stachnis/misc/libhungarian-v0.1.3.tgz
    $ tar -xf libhungarian-v0.1.3.tgz
    $ cd libhungarian
    $ make
    $ sudo cp hungarian.h /usr/local/include
    $ sudo cp libhungarian.a /usr/local/lib

    $ sudo chicken-install srfi-1
    $ sudo chicken-install srfi-4
    $ sudo chicken-install srfi-13
    $ sudo chicken-install srfi-14
    $ sudo chicken-install srfi-69

    $ sudo ln -s /usr/bin/pdflatex /usr/local/bin/pdflatex
    $ sudo ln -s /usr/bin/csc /usr/local/bin/csc5
