compare-elements
----------------
Compares two sets of elements.  
Elements are matched according to their id.

__original-elements__  
A list of original elements.

__original-element-id-procedure__  
A procedure that returns an original element id.

__current-elements__  
A list of current elements.

__current-element-id-procedure__  
A procedure that returns a current element id.

__element-changed?-procedure__  
A procedure invoked with every pair of matching elements.  
Must return whether the element has changed.

__make-added-element-procedure__  
A procedure invoked with every current element that was not matched.  
Must return a value representing its addition.

__make-changed-element-procedure__  
A procedure invoked with every pair of matched elements that have changed.  
Must return a value representing its modification.

__make-unchanged-element-procedure__  
A procedure invoked with every pair of matched elements that have not changed.  
Must return a value representing its non modification.

__make-deleted-element-procedure__  
A procedure invoked with every original element that was not matched.  
Must return a value representing its deletion.

__result__  
A compare-results record composed of the following properties:

- added-elements
- changed-elements
- unchanged-elements
- deleted-elements

try it
------
Place the following code in sources/main.scm.

    (declare (uses compare))

    (define-record employee id name salary)

    (let ((compare-results
            (compare-elements

              ;; original employees
              (list
                (make-employee 1000 "Alice" 20000)
                (make-employee 1001 "Bob" 21000)
                (make-employee 1002 "Carl" 22000))
              employee-id

              ;; current employees
              (list
                (make-employee 1000 "Alice" 20000)
                (make-employee 1001 "Bob" 25000)
                (make-employee 1004 "Dave" 23000))
              employee-id

              ;; checks if an employee has changed
              (lambda (original current)
                (not
                  (and
                    (equal? (employee-name original) (employee-name current))
                    (equal? (employee-salary original) (employee-salary current)))))

              ;; what to do with added employees
              (lambda (original)
                (list
                  "INSERT INTO employees (id, name, salary) VALUES (?1, ?2, ?3);"
                  (employee-id original)
                  (employee-name original)
                  (employee-salary original)))

              ;; what to do with changed employees
              (lambda (original current)
                (list
                  "UPDATE employees SET name = ?2, salary = ?3 WHERE id = ?1;"
                  (employee-id original)
                  (employee-name current)
                  (employee-salary current)))

              ;; what to do with unchanged employees
              (lambda (original current)
                #f)

              ;; what to do with deleted employees
              (lambda (current)
                (list
                  "DELETE FROM employees WHERE id = ?1;"
                  (employee-id current))))))

      (display (compare-results-added-elements compare-results))
      (newline)

      (display (compare-results-changed-elements compare-results))
      (newline)

      (display (compare-results-unchanged-elements compare-results))
      (newline)

      (display (compare-results-deleted-elements compare-results))
      (newline))

Run the following commands.

    $ make
    $ ./main

    ((INSERT INTO employees (id, name, salary) VALUES (?1, ?2, ?3); 1004 Dave 23000))
    ((UPDATE employees SET name = ?2, salary = ?3 WHERE id = ?1; 1001 Bob 25000))
    (#f)
    ((DELETE FROM employees WHERE id = ?1; 1002))
