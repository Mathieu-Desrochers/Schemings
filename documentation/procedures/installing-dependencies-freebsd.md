Installing dependencies on FreeBSD
----------------------------------
Run the following commands.

    $ sudo pkg install chicken5
    $ sudo pkg install fcgi-devkit
    $ sudo pkg install icu
    $ sudo pkg install ImageMagick6
    $ sudo pkg install jansson
    $ sudo pkg install libb64
    $ sudo pkg install libconfig
    $ sudo pkg install libsodium
    $ sudo pkg install libetpan
    $ sudo pkg install libzmq4
    $ sudo pkg install sqlite3
    $ sudo pkg install texlive-full

    $ wget http://www2.informatik.uni-freiburg.de/~stachnis/misc/libhungarian-v0.1.3.tgz
    $ tar -xf libhungarian-v0.1.3.tgz
    $ cd libhungarian
    $ make
    $ sudo cp hungarian.h /usr/local/include
    $ sudo cp libhungarian.a /usr/local/lib

    $ sudo chicken-install5 srfi-1
    $ sudo chicken-install5 srfi-4
    $ sudo chicken-install5 srfi-13
    $ sudo chicken-install5 srfi-14
    $ sudo chicken-install5 srfi-69

    # NOTE: You will need to use gmake
    # NOTE: Modify the makefile to use uctags
