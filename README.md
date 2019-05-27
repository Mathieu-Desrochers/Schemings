You love Scheme
---------------
You understand that simplicity is beautiful, and that Scheme  
is the very essence of what it means to write software.

But somehow committing to an uncommon language appears risky.  
Hard to guarantee the ecosystem will provide a library for all your  
needs down the road. Oh dang, there is no YAML parser...

We fought with this dilemma, and this project is our answer to it.

It is simple really
-------------------
First you pick the Scheme implementation you like the most,  
Chicken Scheme of course. Then you notice it provides an amazing  
foreign interface to call C libraries. And boom it clicks.

We will use the Chicken compiler and get the language we want.  
And we will write simple interfaces to the best C libraries,  
making them feel all Schemy.

The language we love, every library we could ever need.

Many batteries included
-----------------------
We run a commercial web application based on this project.  
Every library we needed is included here. That covers a broad base.

- base64
- configuration files
- crypto
- emails
- fastcgi integration
- http handling
- image processing
- json parsing and formatting
- pdf reports
- regular expressions
- sqlite
- unicode

Getting started
---------------
We provide simple commands to install the required dependencies.  
Simply click on your OS of choice in the following list.

- [Ubuntu](./documentation/procedures/installing-dependencies-ubuntu.md)
- [FreeBSD](.)

Once this is done, you can run the following commands.  
And there you are with a fully operational Scheme environment.

    $ git clone https://github.com/Mathieu-Desrochers/Schemings.git Schemings

    $ cd Schemings
    $ make
    $ ./main

    Hello World

Handy documentation
-------------------
No worries, you are not left alone in the dark. Every  
interface is fully documented, and it provides minimal  
and stand-alone examples.

Simply copy one inside source/main.scm,  
and do the make dance.

    $ make
    $ ./main

    Result from the chosen example.

The complete documentation can be accessed here.

- [Documentation](./documentation/units/)

Is this code any good
---------------------
We think it is. The Scheme code has type annotations all over.  
The C code compiles without warnings, and valgrind appears  
to be happy with our memory management.

Plus our commercial web application is now close to  
one year without runtime errors.
