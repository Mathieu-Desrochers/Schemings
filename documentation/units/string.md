string-mid
----------
Returns a specified number of characters from a string.

__string__  
A source string.

__start__  
The index of the first character.

__length__  
The number of characters.

__result__  
The characters.

string-find-replace
-------------------
Finds and replaces within a string.

__string__  
A string in which to find and replace.

__find__  
A string to be found.

__replace__  
A string to be used as replacement.

__result__  
The replaced string.

try it
------
Place the following code in sources/main.scm.

    (declare (uses string))

    (display
      (string-mid
        "Real stupidity beats artificial intelligence every time"
        15 5))
    (newline)

    (display
      (string-find-replace
        "The password is $password. Keep it to yourself."
        "$password"
        "hunter2"))
    (newline)

Run the following commands.

    $ make
    $ ./main

    beats
    The password is hunter2. Keep it to yourself.
