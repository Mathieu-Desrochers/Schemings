debug-print
-----------
Prints a message to stderr.

__message__  
A message.

try it
------
Place the following code in sources/main.scm.

    (declare (uses debug))

    (debug-print "Everything is awesome!")

Run the following commands.

    $ make
    $ ./main 2> oops
    $ cat oops

    Everything is awesome!
