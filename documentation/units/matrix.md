make-matrix
-----------
Makes a matrix.

__rows-count__  
A number of rows.

__columns-count__  
A number of columns.

matrix-ref
----------
Gets a matrix value.

__matrix__  
A matrix.

__row__  
A row index.

__column__  
A column index.

__result__  
The matrix value.

matrix-set!
-----------
Sets a matrix value.

__matrix__  
A matrix.

__row__  
A row index.

__column__  
A column index.

__value__  
A matrix value.

matrix-rows-count
-----------------
Returns the number of rows in a matrix.

__matrix__  
A matrix.

__result__  
The number of rows.

matrix-columns-count
--------------------
Returns the number of columns in a matrix.

__matrix__  
A matrix.

__result__  
The number of columns.

matrix-maximum-assignment
-------------------------
Solves the maximum assignment problem using the hungarian method.

__matrix__  
A matrix.

__result__  
The list of row and column pairs having the maximum value.

matrix->string
--------------
Returns the string representation of a matrix.

__matrix__  
A matrix.

__result__  
The matrix as a string.

try it
------
Place the following code in sources/main.scm.

    (declare (uses matrix))

    (let ((matrix (make-matrix 4 3)))

      (matrix-set! matrix 0 0 100)
      (matrix-set! matrix 0 1 1)
      (matrix-set! matrix 0 2 1)

      (matrix-set! matrix 1 0 100)
      (matrix-set! matrix 1 1 2)
      (matrix-set! matrix 1 2 2)

      (matrix-set! matrix 2 0 1)
      (matrix-set! matrix 2 1 0)
      (matrix-set! matrix 2 2 0)

      (matrix-set! matrix 3 0 0)
      (matrix-set! matrix 3 1 2)
      (matrix-set! matrix 3 2 0)

      (display (matrix->string matrix))
      (newline)
      (newline)

      (display (matrix-maximum-assignment matrix))
      (newline))

Run the following commands.

    $ make
    $ ./main

    [ 100, 1, 1 ]
    [ 100, 2, 2 ]
    [ 1, 0, 0 ]
    [ 0, 2, 0 ]

    ((0 . 0) (1 . 2) (3 . 1))

powered by
----------
The great libhungarian.
