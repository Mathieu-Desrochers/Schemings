(import srfi-1)

(import (chicken condition))
(import (chicken format))

(declare (unit sql-sqlite3))

(declare (uses exceptions))
(declare (uses sql-intern))
(declare (uses sqlite3))

;; binds a parameter of a sqlite3-stmt*
(: sql-bind-parameter-sqlite3 (pointer fixnum * -> noreturn))
(define (sql-bind-parameter-sqlite3 sqlite3-stmt* parameter-number parameter-value)
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
(: sql-bind-parameters-sqlite3 (pointer (list-of *) -> noreturn))
(define (sql-bind-parameters-sqlite3 sqlite3-stmt* parameter-values)
  (for-each
    (lambda (parameter-index)
      (let* ((parameter-number (+ parameter-index 1))
             (parameter-value (list-ref parameter-values parameter-index))
             (sql-bind-parameter-sqlite3-result
               (sql-bind-parameter-sqlite3 sqlite3-stmt* parameter-number parameter-value)))
        (unless (= sql-bind-parameter-sqlite3-result sqlite3-result-ok)
          (abort
            (format "failed to bind value ~A to parameter ?~A with error code ~A"
              parameter-value
              parameter-number
              sql-bind-parameter-sqlite3-result)))))
    (iota (length parameter-values))))

;; reads a column from a sqlite3-stmt*
(: sql-read-column-sqlite3 (pointer fixnum -> *))
(define (sql-read-column-sqlite3 sqlite3-stmt* column-index)
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

;; reads all the rows from a sqlite3-stmt*
(: sql-read-all-rows-sqlite3 (pointer -> (list-of (list-of *))))
(define (sql-read-all-rows-sqlite3 sqlite3-stmt*)
  (letrec* ((columns-count (sqlite3-column-count sqlite3-stmt*))
            (sql-read-row-sqlite3
              (lambda ()
                (map
                  (lambda (column-index)
                    (sql-read-column-sqlite3 sqlite3-stmt* column-index))
                  (iota columns-count))))
            (accumulate-rows
              (lambda (rows)
                (let ((sqlite3-step-result (sqlite3-step sqlite3-stmt*)))
                  (unless (or (= sqlite3-step-result sqlite3-result-row)
                              (= sqlite3-step-result sqlite3-result-done))
                    (abort (format "failed to read next row with error code ~A" sqlite3-step-result)))
                  (if (= sqlite3-step-result sqlite3-result-row)
                    (let ((row (sql-read-row-sqlite3)))
                      (accumulate-rows (cons row rows)))
                    rows)))))
    (let ((rows (accumulate-rows '())))
      (reverse rows))))

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
          (sql-bind-parameters-sqlite3 sqlite3-stmt* parameter-values)
          (procedure sqlite3-stmt*))
        (lambda (sqlite3-stmt*)
          (sqlite3-finalize sqlite3-stmt*))))
    (lambda (sqlite3-stmt**)
      (free-sqlite3-stmt* sqlite3-stmt**))))

;; invokes a procedure with a sql connection
(: with-sql-connection-sqlite3 (forall (r) (string ((struct sql-connection) -> r) -> r)))
(define (with-sql-connection-sqlite3 connection-string procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((sqlite3** (malloc-sqlite3*)))
        (unless sqlite3**
          (abort "failed to allocate sqlite3*"))
        sqlite3**))
    (lambda (sqlite3**)
      (let ((sqlite3-open-result (sqlite3-open database-name sqlite3**)))
        (unless (eq? sqlite3-open-result sqlite3-result-ok)
          (abort
            (format "failed to open database ~A with error code ~A"
              database-name
              sqlite3-open-result)))
        (let ((sql-connection (make-sql-connection #f (resolve-sqlite3* sqlite3**))))
          (procedure sql-connection))))
    (lambda (sqlite3**)
      (sqlite3-close-v2 (resolve-sqlite3* sqlite3**))
      (free-sqlite3* sqlite3**))))

;; executes a sql statement
(: sql-execute-sqlite3 ((struct sql-connection) string (list-of *) -> noreturn))
(define (sql-execute-sqlite3 sql-connection statement parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (let ((sqlite3-step-result (sqlite3-step sqlite3-stmt*)))
          (if (= sqlite3-step-result sqlite3-result-busy)
            (sql-raise-deadlock-exception))
          (if (not (= sqlite3-step-result sqlite3-result-done))
            (abort
              (format "failed to execute statement ~A with error code ~A"
                statement
                sqlite3-step-result))))))))

;; executes a sql statement that returns rows
(: sql-read-sqlite3 ((struct sql-connection) string list -> (list-of (list-of *))))
(define (sql-read-sqlite3 sql-connection statement parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sql-read-all-rows-sqlite3 sqlite3-stmt*)))))
