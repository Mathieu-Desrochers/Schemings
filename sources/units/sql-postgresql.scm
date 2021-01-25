(import srfi-1)

(import (chicken condition))
(import (chicken format))

(declare (unit sql-postgresql))

(declare (uses date-time))
(declare (uses exceptions))
(declare (uses postgresql))

;; formats a parameter value
(: sql-format-parameter-value-postgresql (* -> string))
(define (sql-format-parameter-value-postgresql parameter-value)
  (cond ((boolean? parameter-value) (if parameter-value "true" "false"))
        ((number? parameter-value) (number->string parameter-value))
        ((string? parameter-value) parameter-value)
        ((date? parameter-value) (date->string parameter-value))
        ((date-time? parameter-value) (format-date-time parameter-value "%Y-%m-%d %H:%M:%S"))
        ((time? parameter-value) (time->string* parameter-value))
        ((eq? parameter-value 'null) #f)
        (else
          (abort
            (format "failed to format value ~A"
              parameter-value)))))

;; invokes a procedure with a params-array**
(: with-sql-postgresql-params-array** (forall (r) ((list-of *) ((struct params-array**) -> r) -> r)))
(define (with-sql-postgresql-params-array** parameter-values procedure)
  (with-guaranteed-release
    (lambda ()
      (postgresql-make-params-array (length parameter-values)))
    (lambda (params-array**)
      (for-each
        (lambda (parameter-index)
          (let* ((parameter-value (list-ref parameter-values parameter-index))
                 (parameter-value-formatted (sql-format-parameter-value-postgresql parameter-value)))
            (postgresql-set-params-array params-array** parameter-index parameter-value-formatted)))
        (iota (length parameter-values)))
      (procedure params-array**))
    (lambda (params-array**)
      (postgresql-free-params-array params-array** (length parameter-values)))))

;; parses a value from a pgresult*
(: sql-parse-result-value-postgresql (pointer fixnum fixnum -> *))
(define (sql-parse-result-value-postgresql pgresult* row-index column-index)
  (let ((column-type (pqftype pgresult* column-index))
        (result-value (pqgetvalue pgresult* row-index column-index)))
    (cond ((= column-type postgresql-type-bool)
            (equal? result-value "t"))
          ((or (= column-type postgresql-type-int4)
               (= column-type postgresql-type-numeric)
               (= column-type postgresql-type-float8))
            (string->number result-value))
          ((or (= column-type postgresql-type-char)
               (= column-type postgresql-type-text))
            result-value)
          ((= column-type postgresql-type-date)
            (string->date result-value))
          ((= column-type postgresql-type-timestamp)
            (parse-date-time result-value "%Y-%m-%d %H:%M:%S"))
          ((= column-type postgresql-type-time)
            (string->time* result-value))
          (else
            (abort
              (format
                "failed to parse column at index ~A of type ~A"
                column-index
                column-type))))))

;; reads all the rows from a pgresult*
(: sql-read-all-rows-postgresql (pointer -> (list-of (list-of *))))
(define (sql-read-all-rows-postgresql pgresult*)
  (let* ((rows-count (pqntuples pgresult*))
         (columns-count (pqnfields pgresult*))
         (sql-read-row-postgresql
            (lambda (row-index)
              (map
                (lambda (column-index)
                  (sql-parse-result-value-postgresql pgresult* row-index column-index))
                (iota columns-count)))))
    (map
      sql-read-row-postgresql
      (iota rows-count))))

;; invokes a procedure with a pgresult*
(: with-pgresult* (forall (r) (pointer string (list-of *) (pointer -> r) -> r)))
(define (with-pgresult* pgconn* statement parameter-values procedure)
  (with-sql-postgresql-params-array** parameter-values
    (lambda (params-array**)
      (with-guaranteed-release
        (lambda ()
          (pqexecparams
            pgconn* statement (length parameter-values) #f
            params-array** #f #f 0))
        procedure
        pqclear))))

;; invokes a procedure with a sql connection
(: with-sql-connection-postgresql (forall (r) (string ((struct sql-connection) -> r) -> r)))
(define (with-sql-connection-postgresql connection-string procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((pgconn* (pqconnectdb connection-string)))
        (unless (eq? (pqstatus pgconn*) pqstatus-connection-ok)
          (abort
            (format "failed to open database ~A with error ~A"
              connection-string
              (pqerrormessage pgconn*))))
        pgconn*))
    (lambda (pgconn*)
      (let ((sql-connection (make-sql-connection pgconn* #f)))
        (procedure sql-connection)))
    (lambda (pgconn*)
      (pqfinish pgconn*))))

;; executes a sql statement
(: sql-execute-postgresql ((struct sql-connection) string (list-of *) -> noreturn))
(define (sql-execute-postgresql sql-connection statement parameter-values)
  (let ((pgconn* (sql-connection-pgconn* sql-connection)))
    (with-pgresult* pgconn* statement parameter-values
      (lambda (pgresult*)
        (unless (eq? (pqresultstatus pgresult*) pgres-command-ok)
          (abort
            (format "failed to execute statement ~A with error ~A"
              statement
              (pqresulterrormessage pgresult*))))))))

;; executes a sql statement that returns rows
(: sql-read-postgresql ((struct sql-connection) string list -> (list-of (list-of *))))
(define (sql-read-postgresql sql-connection statement parameter-values)
  (let ((pgconn* (sql-connection-pgconn* sql-connection)))
    (with-pgresult* pgconn* statement parameter-values
      (lambda (pgresult*)
        (unless (eq? (pqresultstatus pgresult*) pgres-tuples-ok)
          (abort
            (format "failed to read statement ~A with error ~A"
              statement
              (pqresulterrormessage pgresult*))))
        (sql-read-all-rows-postgresql pgresult*)))))
