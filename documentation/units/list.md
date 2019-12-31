list-duplicates-index
---------------------
Returns the index of the elements that appear more than once in a list.  
Ignores the elements having a false value.

__elements__  
The list of elements.

__element-value-procedure__  
A procedure that returns the value of a list element.

__result__  
The index of the duplicated elements.

list-matches-index
------------------
Returns the index of the elements in a first list  
whose value can be matched in a second list.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

__second-elements__  
The second list of elements.

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

__result__  
The index of the matched elements.

list-non-matches-index
----------------------
Returns the index of the elements in a first list  
whose value cannot be matched in a second list.  
Ignores the elements having a false value.

__first-elements__  
The first list of elements.

__first-element-value-procedure__  
A procedure that returns the value of a first list element.

__second-elements__  
The second list of elements.

__second-element-value-procedure__  
A procedure that returns the value of a second list element.

__result__  
The index of the unmatched elements.

list-distinct-values
--------------------
Returns the distinct values from a list.  
Ignores the elements having a false value.

__elements__  
The list of elements.

__element-value-procedure__  
A procedure that returns the value of a list element.

__result__  
The distinct values.

list-filtered-index
-------------------
Returns the index of the elements whose value matches a filter.

__elements__  
The list of elements.

__element-value-procedure__  
A procedure that returns the value of a list element.

__filter-procedure__  
A filter procedure.

__result__  
The index of the filtered elements.

list-is-sequential
------------------
Returns whether the value of the elements form a sequence starting from one.  
The values do not need to appear in any order.

__elements__  
The list of elements.

__element-value-procedure__  
A procedure that returns the value of a list element.

__result__  
Whether the elements form a sequence.

list-for-each-with-index
------------------------
Invokes a procedure with each element of a list  
along with its index.

__elements__  
The list of elements.

__procedure__  
A procedure to invoke.

list-split
----------
Splits a list in sublists.

__elements__  
The list of elements.

__sublist-size__  
A size for the sublists.

__result__  
The list of sublists.

try it
------
Place the following code in sources/main.scm.

    (declare (uses list))

    (define-record employee id name salary)

    (display "employees having duplicated ids: ")
    (display
      (list-duplicates-index
        (list
          (make-employee 1000 "Alice" 20000)
          (make-employee 1000 "Bob" 21000)
          (make-employee 1001 "Carl" 22000)
          (make-employee 1002 "Dave" 23000)
          (make-employee 1003 "Eve" 24000)
          (make-employee 1003 "Frank" 25000))
        employee-id))
    (newline)

    (display "employees missing from the second list: ")
    (display
      (list-non-matches-index
        (list
          (make-employee 1000 "Alice" 20000)
          (make-employee 1001 "Bob" 21000)
          (make-employee 1002 "Carl" 22000))
        employee-id
        (list
          (make-employee 1000 "Alice" 20000)
          (make-employee 1001 "Bob" 21000))
        employee-id))
    (newline)

    (display "employees unique names: ")
    (display
      (list-distinct-values
        (list
          (make-employee 1000 "Alice" 20000)
          (make-employee 1001 "Bob" 21000)
          (make-employee 1002 "Carl" 22000)
          (make-employee 1003 "Alice" 23000))
        employee-name))
    (newline)

    (display "employees being rich: ")
    (display
      (list-filtered-index
        (list
          (make-employee 1000 "Alice" 20000)
          (make-employee 1001 "Bob" 21000)
          (make-employee 1002 "Carl" 22000)
          (make-employee 1003 "Alice" 23000))
        employee-salary
        (lambda (salary)
          (> salary 21500))))
    (newline)

    (display "employees with index: ")
    (list-for-each-with-index
      (list
        (make-employee 1000 "Alice" 20000)
        (make-employee 1001 "Bob" 21000)
        (make-employee 1002 "Carl" 22000))
      (lambda (employee index)
        (display (cons (employee-name employee) index))))
    (newline)

Run the following commands.

    $ make
    $ ./main

    employees having duplicated ids: (0 1 4 5)
    employees missing from the second list: (2)
    employees unique names: (Alice Bob Carl)
    employees being rich: (2 3)
    employees with index: (Alice . 0)(Bob . 1)(Carl . 2)
