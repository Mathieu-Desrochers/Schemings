(import srfi-13)
(import srfi-14)

(import (chicken condition))
(import (chicken format))

(declare (unit sql))

(declare (uses date-time))
(declare (uses debug))
(declare (uses exceptions))
(declare (uses sql-intern))
(declare (uses postgresql))
(declare (uses utf8))

;; encapsulates a sql connection
(define-typed-record sql-connection
  (pgconn* pointer))

;; invokes a procedure with a sql connection
(: with-sql-connection (forall (r) (string ((struct sql-connection) -> r) -> r)))
(define (with-sql-connection connection-string procedure)
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
      (let ((sql-connection (make-sql-connection pgconn*)))
        (procedure sql-connection)))
    (lambda (pgconn*)
      (pqfinish pgconn*))))

;; executes a procedure within a transaction
;; the transaction is automatically rollbacked if an exception occurs
(: within-sql-transaction (forall (r) ((struct sql-connection) (-> r) -> r)))
(define (within-sql-transaction sql-connection procedure)
  (sql-execute sql-connection "BEGIN TRANSACTION;" (list))
  (handle-exceptions
    exception
    (begin
      (with-exception-hiding
        (lambda ()
          (sql-execute sql-connection "ROLLBACK TRANSACTION;" (list))))
        (abort exception))
    (let ((procedure-result (procedure)))
      (sql-execute sql-connection "COMMIT TRANSACTION;" (list))
      procedure-result)))

;; executes a sql statement
(: sql-execute ((struct sql-connection) string (list-of *) -> noreturn))
(define (sql-execute sql-connection statement parameter-values)
  (let ((pgconn* (sql-connection-pgconn* sql-connection)))
    (with-pgresult* pgconn* statement parameter-values
      (lambda (pgresult*)
        (unless (eq? (pqresultstatus pgresult*) pgres-command-ok)
          (abort
            (format "failed to execute statement ~A with error ~A"
              statement
              (pqresulterrormessage pgresult*))))))))

;; executes a sql statement that returns rows
(: sql-read ((struct sql-connection) string list -> (list-of (list-of *))))
(define (sql-read sql-connection statement parameter-values)
  (let ((pgconn* (sql-connection-pgconn* sql-connection)))
    (with-pgresult* pgconn* statement parameter-values
      (lambda (pgresult*)
        (unless (eq? (pqresultstatus pgresult*) pgres-tuples-ok)
          (abort
            (format "failed to read statement ~A with error ~A"
              statement
              (pqresulterrormessage pgresult*))))
        (sql-read-all-rows pgresult*)))))

;; returns the id generated for the last inserted row
(: sql-last-generated-id ((struct sql-connection) -> fixnum))
(define (sql-last-generated-id sql-connection)
  (caar (sql-read sql-connection "SELECT lastval();" (list))))

;; returns a string to be used for accent and case
;; insensitive searches using the like value% construct
(: sql-searchable-string (string -> string))
(define (sql-searchable-string string)
  (utf8-lower-case
    (utf8-remove-accents
      (string-delete
        (string->char-set "%_")
        string))))

;; executes a procedure until no deadlock occurs
;; or a maximum number of retries is reached
(define (with-sql-deadlock-retries count procedure)
  (letrec (
      (with-sql-retry-on-deadlock-inner
        (lambda (inner-count)
          (handle-exceptions
            exception
            (begin
              (if (sql-deadlock-exception? exception)
                (if (> inner-count count)
                  (begin
                    (debug-print (format "DEADLOCK could not be resolved after ~A retries" count))
                    (abort exception))
                  (begin
                    (debug-print (format "DEADLOCK occured on try ~A" inner-count))
                    (with-sql-retry-on-deadlock-inner (+ inner-count 1))))
                (abort exception)))
            (procedure)))))
    (with-sql-retry-on-deadlock-inner 1)))

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

;; executes a procedure outside of the current transaction
;; the transaction is committed then restarted
(: without-sql-transaction (forall (r) ((struct sql-connection) (-> r) -> r)))
(define (without-sql-transaction sql-connection procedure)
  (with-guaranteed-release
    (lambda ()
      (sql-execute sql-connection "COMMIT TRANSACTION;" (list)))
    (lambda (_)
      (procedure))
    (lambda (_)
      (sql-execute sql-connection "BEGIN TRANSACTION;" (list)))))
