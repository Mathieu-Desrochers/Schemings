math-round
----------
Rounds a value to a number of decimals.

__value__  
A number.

__decimals-count__  
A number of decimals.

__result__  
The rounded value.

math-cumulative-moving-average-add-value
----------------------------------------
Adds a new value to a cumulative moving average.

__average__  
A current average.

__n__  
A current number of values.

__value__  
A new value.

__result__  
The new average.

math-cumulative-moving-average-remove-value
-------------------------------------------
Removes a value from a cumulative moving average.

__average__  
A current average.

__n__  
A current number of values.

__value__  
A removed value.

__result__  
The new average.

math-calculate-age
------------------
Returns the number of years between two dates.  
Considers completed months only.

__date-from__  
A from date.

__days-to__  
A to date.

__result__  
The calculated age.

try it
------
Place the following code in sources/main.scm.

    (declare (uses date-time))
    (declare (uses math))

    (display (math-round (/ 1 3) 2))
    (newline)

    (display (math-cumulative-moving-average-add-value 250 5 10))
    (newline)

    (display (math-cumulative-moving-average-remove-value 210 6 10))
    (newline)

    (display (math-calculate-age (make-date 2001 02 03) (date-today)))
    (newline)

Run the following commands.

    $ make
    $ ./main

    0.33
    210
    250
    18.31

powered by
----------
The power of mathematics.
