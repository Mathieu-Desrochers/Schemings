vector-concatenate
------------------
Concatenates a list of vectors.

__vectors__  
A list of vectors.

__length__  
A number of elements to keep from the concatenated vector.

__result__  
The concatenated vector.

try it
------
Place the following code in sources/main.scm.

    (import srfi-4)

    (declare (uses vector))

    (display
      (vector-concatenate
        (list
          (make-u8vector 2 100)
          (make-u8vector 3 200))
        4))
    (newline)

Run the following commands.

    $ make
    $ ./main

    #u8(100 100 200 200)
