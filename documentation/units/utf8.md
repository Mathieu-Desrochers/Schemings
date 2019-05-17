utf8-length
-----------
Returns a utf8 string length.

__string__  
A utf8 string.

__result__  
The string length.

utf8-upper-case
---------------
Upper cases a utf8 string.

__string__  
A utf8 string.

__result__  
The upper cased string.

utf8-lower-case
---------------
Lower cases a utf8 string.

__string__  
A utf8 string.

__result__  
The lower cased string.

utf8-remove-accents
-------------------
Removes the accents from a utf8 string.

__string__  
A utf8 string.

__result__  
The string without accents.

utf8-change-script
------------------
Returns a utf8 string in a different script.

__source-script__  
A source script.

__destination-script__  
A destination script.

__string__  
A utf8 string.

__result__  
The string in the destination script.

try it
------
Place the following code in sources/main.scm.

    (declare (uses utf8))

    (display (utf8-length "Blåbærsyltetøy"))
    (newline)

    (display (utf8-upper-case "Τη γλώσσα μου έδωσαν ελληνική το σπίτι"))
    (newline)

    (display (utf8-remove-accents "Maître Corbeau, sur un arbre perché"))
    (newline)

    (display (utf8-change-script "Russian/BGN" "Latin" "На берегу пустынных"))
    (newline)

Run the following commands.

    $ make
    $ ./main

    14
    ΤΗ ΓΛΏΣΣΑ ΜΟΥ ΈΔΩΣΑΝ ΕΛΛΗΝΙΚΉ ΤΟ ΣΠΊΤΙ
    Maitre Corbeau, sur un arbre perche
    Na beregu pustynnykh

powered by
----------
The great libicu.
