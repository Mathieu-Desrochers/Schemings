(import srfi-1)

(import (chicken condition))
(import (chicken format))

(declare (unit sql-intern))

(declare (uses date-time))
(declare (uses exceptions))
(declare (uses sqlite3))

;; binds a parameter of a sqlite3-stmt*
(: sql-bind-parameter (pointer fixnum * -> noreturn))
(define (sql-bind-parameter sqlite3-stmt* parameter-number parameter-value)
  (cond ((and (integer? parameter-value) (exact? parameter-value))
          (sqlite3-bind-int sqlite3-stmt* parameter-number parameter-value))
        ((number? parameter-value)
          (sqlite3-bind-double sqlite3-stmt* parameter-number parameter-value))
        ((string? parameter-value)
          (sqlite3-bind-text sqlite3-stmt* parameter-number parameter-value -1 sqlite3-transient))
        ((not parameter-value)
          (sqlite3-bind-null sqlite3-stmt* parameter-number))
        (else
          (abort
            (format "failed to bind value ~A to parameter ?~A"
              parameter-value
              parameter-number)))))

;; binds the parameters of a sqlite3-stmt*
(: sql-bind-parameters (pointer (list-of *) -> noreturn))
(define (sql-bind-parameters sqlite3-stmt* parameter-values)
  (for-each
    (lambda (parameter-index)
      (let* ((parameter-number (+ parameter-index 1))
             (parameter-value (list-ref parameter-values parameter-index))
             (sql-bind-parameter-result
               (sql-bind-parameter sqlite3-stmt* parameter-number parameter-value)))
        (unless (= sql-bind-parameter-result sqlite3-result-ok)
          (abort
            (format "failed to bind value ~A to parameter ?~A with error code ~A"
              parameter-value
              parameter-number
              sql-bind-parameter-result)))))
    (iota (length parameter-values))))

;; invokes a procedure with a sqlite3-stmt*
(: with-sqlite3-stmt* (forall (r) (pointer string (list-of *) (pointer -> r) -> r)))
(define (with-sqlite3-stmt* sqlite3* statement parameter-values procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((sqlite3-stmt** (malloc-sqlite3-stmt*)))
        (unless sqlite3-stmt**
          (abort "failed to allocate sqlite3-stmt*"))
        sqlite3-stmt**))
    (lambda (sqlite3-stmt**)
      (with-guaranteed-release
        (lambda ()
          (let ((sqlite3-prepare-v2-result (sqlite3-prepare-v2 sqlite3* statement -1 sqlite3-stmt** #f)))
            (unless (eq? sqlite3-prepare-v2-result sqlite3-result-ok)
              (abort (format "failed to prepare statement ~A" statement)))
            (resolve-sqlite3-stmt* sqlite3-stmt**)))
        (lambda (sqlite3-stmt*)
          (sql-bind-parameters sqlite3-stmt* parameter-values)
          (procedure sqlite3-stmt*))
        (lambda (sqlite3-stmt*)
          (sqlite3-finalize sqlite3-stmt*))))
    (lambda (sqlite3-stmt**)
      (free-sqlite3-stmt* sqlite3-stmt**))))

;; reads a column from a sqlite3-stmt*
(: sql-read-column (pointer fixnum -> *))
(define (sql-read-column sqlite3-stmt* column-index)
  (let ((column-type (sqlite3-column-type sqlite3-stmt* column-index)))
    (cond ((= column-type sqlite3-type-integer)
            (sqlite3-column-int sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-float)
            (sqlite3-column-double sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-text)
            (sqlite3-column-text sqlite3-stmt* column-index))
          ((= column-type sqlite3-type-null) #f)
          (else
            (abort (format "failed to read column at index ~A" column-index))))))

;; reads all the rows from a sql-stmt*
(: sql-read-all-rows (pointer -> (list-of (list-of *))))
(define (sql-read-all-rows sqlite3-stmt*)
  (letrec* ((columns-count (sqlite3-column-count sqlite3-stmt*))
            (sql-read-row
              (lambda ()
                (map
                  (lambda (column-index)
                    (sql-read-column sqlite3-stmt* column-index))
                  (iota columns-count))))
            (accumulate-rows
                (lambda (rows)
                  (let ((sqlite3-step-result (sqlite3-step sqlite3-stmt*)))
                    (unless (or (= sqlite3-step-result sqlite3-result-row)
                                (= sqlite3-step-result sqlite3-result-done))
                      (abort (format "failed to read next row with error code ~A" sqlite3-step-result)))
                    (if (= sqlite3-step-result sqlite3-result-row)
                      (let ((row (sql-read-row)))
                        (accumulate-rows (cons row rows)))
                      rows)))))
    (let ((rows (accumulate-rows '())))
      (reverse rows))))

;; raises a deadlock exception
(: sql-raise-deadlock-exception (-> noreturn))
(define (sql-raise-deadlock-exception)
  (let ((condition (make-property-condition 'sql-deadlock)))
    (abort condition)))

;; returns whether an exception was caused by a deadlock
(: sql-deadlock-exception? (condition -> boolean))
(define (sql-deadlock-exception? exception)
  ((condition-predicate 'sql-deadlock)
    exception))

;; downgrades a value to a database supported format
(: table-value->sql-value (* symbol -> *))
(define (table-value->sql-value value value-type)
  (cond ((eq? value-type 'boolean) (if value 1 0))
        ((not value) #f)
        ((eq? value-type 'date) (date->string value))
        ((eq? value-type 'date-time) (date-time->string value))
        ((eq? value-type 'time) (time->string* value))
        (else value)))

;; upgrades a value from a database supported format
(: sql-value->table-value (* symbol -> *))
(define (sql-value->table-value value value-type)
  (cond ((eq? value-type 'boolean) (eq? value 1))
        ((not value) #f)
        ((eq? value-type 'date) (or (try-string->date value) 'invalid-value))
        ((eq? value-type 'date-time) (or (try-string->date-time value) 'invalid-value))
        ((eq? value-type 'time) (or (try-string->time value) 'invalid-value))
        (else value)))
