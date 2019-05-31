sort-by-number
--------------
Sorts a list of elements by their numeric value.

__elements__  
A list of elements.

__element-sort-value-procedure__  
A procedure that returns the numeric value of an element.

__result__  
The list of sorted elements.

sort-by-number-descending
-------------------------
Same as above but in descending order.

sort-by-string
--------------
Sorts a list of elements by their string value.

__elements__  
A list of elements.

__element-sort-value-procedure__  
A procedure that returns the string value of an element.

__result__  
The list of sorted elements.

sort-by-string-descending
-------------------------
Same as above but in descending order.

try it
------
Place the following code in sources/main.scm.

    (declare (uses sort))

    (display
      (sort-by-number
        (list
          (cons 1000 "a thousand")
          (cons 1002 "a thousand and two")
          (cons 1001 "a thousand and one"))
        car))
    (newline)

    (display
      (sort-by-string-descending
        (list
          (cons 1000 "yes")
          (cons 1001 "heck no")
          (cons 1002 "maybe"))
        cdr))
    (newline)

Run the following commands.

    $ make
    $ ./main

    ((1000 . a thousand) (1001 . a thousand and one) (1002 . a thousand and two))
    ((1000 . yes) (1002 . maybe) (1001 . heck no))
