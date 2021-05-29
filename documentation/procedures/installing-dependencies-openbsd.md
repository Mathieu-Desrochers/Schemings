Installing dependencies on OpenBSD
----------------------------------
Run the following commands.

    $ doas pkg_add chicken
    $ doas pkg_add czmq
    $ doas pkg_add fcgi
    $ doas pkg_add ImageMagick
    $ doas pkg_add icu4c
    $ doas pkg_add jansson
    $ doas pkg_add libconfig
    $ doas pkg_add libsodium
    $ doas pkg_add libetpan
    $ doas pkg_add libev
    $ doas pkg_add spawn-fcgi
    $ doas pkg_add sqlite3
    $ doas pkg_add texlive_texmf-full

    $ unzip libraries/libb64-1.2.src.zip
    $ cd libb64-1.2
    $ gmake
    $ doas cp include/b64/cdecode.h /usr/local/include
    $ doas cp include/b64/cencode.h /usr/local/include
    $ doas cp src/libb64.a /usr/local/lib

    $ tar -xzf libraries/libhungarian-v0.1.3.tgz
    $ cd libhungarian
    $ gmake
    $ doas cp hungarian.h /usr/local/include
    $ doas cp libhungarian.a /usr/local/lib

    $ tar -xzf multiwatch-1.0.0.tar.gz
    $ cd multiwatch-1.0.0
    $ ./configure CFLAGS="-I/usr/local/include -L/usr/local/lib"
    $ gmake
    $ doas gmake install

    $ unzip libraries/statsd-c-client-master.zip
    $ cd statsd-c-client-master
    $ gmake
    $ doas gmake install

    $ doas chicken-install srfi-1
    $ doas chicken-install srfi-4
    $ doas chicken-install srfi-13
    $ doas chicken-install srfi-14
    $ doas chicken-install srfi-18
    $ doas chicken-install srfi-69
