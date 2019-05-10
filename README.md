You love Scheme
---------------
You understand that simplicity is beautiful, and that Scheme is the very essence of what it means to write software.

But somehow committing to an uncommon language appears risky. Hard to guarantee the ecosystem will provide a library for all your needs down the road. Oh dang, there is no YAML parser...

We fought with this dilemma, and this project is our answer to it.

It is simple really
-------------------
First you pick the Scheme implementation you like the most, Chicken Scheme of course. Then you notice it provides an amazing foreign interface to call C libraries. And boom it clicks.

We will use the Chicken compiler and get the language we want. And we will write simple bindings to the best C libraries, making them feel all Schemy.

The language we love, every library we could ever need.

Many batteries included
-----------------------
We run a commercial web application based on this project.  
Every library we needed is included here. That covers a broad base.

- base64 encoding
- configuration
- crypto
- emails sending
- fastcgi integration
- http handling
- i18n
- image processing
- json parsing and formatting
- regular expressions
- sqlite

Getting started
---------------
On Ubuntu run the following commands.

    $ sudo apt-get install gcc
    $ sudo apt-get install libb64-dev
    $ sudo apt-get install libsodium-dev
    $ sudo apt-get install libc6-dev
    $ sudo apt-get install make

    $ wget https://code.call-cc.org/releases/5.0.0/chicken-5.0.0.tar.gz
    $ tar -xf chicken-5.0.0.tar.gz
    $ cd chicken-5.0.0
    $ make PLATFORM=linux
    $ sudo make PLATFORM=linux install
    $ sudo ln /usr/local/bin/csc /usr/local/bin/csc5
    $ sudo ln /usr/local/bin/chicken-install /usr/local/bin/chicken-install5

    $ sudo chicken-install srfi-13
    $ sudo chicken-install srfi-14

    $ git clone https://github.com/Mathieu-Desrochers/Schemings.git Schemings

    $ cd Schemings
    $ make
    $ ./main

    Hello World
