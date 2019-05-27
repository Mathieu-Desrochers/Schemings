hash-by-unique-key
------------------
Hashes elements by a unique numeric key.

__elements__  
The list of elements.

__element-key-procedure__  
A procedure that returns an element key.

__element-value-procedure__  
A procedure that returns an element value.

hash-by-unique-string-key
-------------------------
Hashes elements by a unique string key.

__elements__  
The list of elements.

__element-key-procedure__  
A procedure that returns an element key.

__element-value-procedure__  
A procedure that returns an element value.

hash-by-shared-key
------------------
Hashes elements by a shared numeric key.

__elements__  
The list of elements.

__element-key-procedure__  
A procedure that returns an element key.

__element-value-procedure__  
A procedure that returns an element value.

hash-by-shared-string-key
-------------------------
Hashes elements by a shared string key.

__elements__  
The list of elements.

__element-key-procedure__  
A procedure that returns an element key.

__element-value-procedure__  
A procedure that returns an element value.

try it
------
Place the following code in sources/main.scm.

    (import srfi-69)

    (declare (uses hash))

    (define-record employee id name salary)

    (let ((employees-hash-table
            (hash-by-unique-key
              (list
                (make-employee 1000 "Alice" 20000)
                (make-employee 1001 "Bob" 21000))
              employee-id
              employee-name)))
      (display (hash-table-ref employees-hash-table 1000))
      (newline))

    (let ((employees-hash-table
            (hash-by-shared-string-key
              (list
                (make-employee 1000 "Alice" 20000)
                (make-employee 1001 "Bob" 21000)
                (make-employee 1002 "Alice" 22000))
              employee-name
              employee-id)))
      (display (hash-table-ref employees-hash-table "Alice"))
      (newline))

Run the following commands.

    $ make
    $ ./main

    Alice
    (1000 1002)
