Installing dependencies on Ubuntu
---------------------------------
Run the following commands.

    $ sudo apt install exuberant-ctags
    $ sudo apt install gcc
    $ sudo apt install libb64-dev
    $ sudo apt install libconfig-dev
    $ sudo apt install libcurl4-openssl-dev
    $ sudo apt install libetpan-dev
    $ sudo apt install libfcgi-dev
    $ sudo apt install libicu-dev
    $ sudo apt install libjansson-dev
    $ sudo apt install libpcre3-dev
    $ sudo apt install libsodium-dev
    $ sudo apt install libc6-dev
    $ sudo apt install make

    $ wget https://code.call-cc.org/releases/5.0.0/chicken-5.0.0.tar.gz
    $ tar -xf chicken-5.0.0.tar.gz
    $ cd chicken-5.0.0
    $ make PLATFORM=linux
    $ sudo make PLATFORM=linux install
    $ sudo ln /usr/local/bin/csc /usr/local/bin/csc5

    $ sudo chicken-install srfi-1
    $ sudo chicken-install srfi-4
    $ sudo chicken-install srfi-13
    $ sudo chicken-install srfi-14
    $ sudo chicken-install srfi-69
