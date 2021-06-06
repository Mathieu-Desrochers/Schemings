(import srfi-13)
(import srfi-14)

(import (chicken condition))
(import (chicken format))

(declare (unit sql))

(declare (uses date-time))
(declare (uses debug))
(declare (uses exceptions))
(declare (uses sql-intern))
(declare (uses sqlite3))
(declare (uses utf8))

;; encapsulates a sql connection
(define-typed-record sql-connection
  (sqlite3* pointer))

;; invokes a procedure with a sql connection
(: with-sql-connection (forall (r) (string ((struct sql-connection) -> r) -> r)))
(define (with-sql-connection database-name procedure)
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
        (let ((sql-connection (make-sql-connection (resolve-sqlite3* sqlite3**))))
          (procedure sql-connection))))
    (lambda (sqlite3**)
      (sqlite3-close-v2 (resolve-sqlite3* sqlite3**))
      (free-sqlite3* sqlite3**))))

;; executes a procedure within a transaction
;; the transaction is automatically rollbacked if an exception occurs
(: within-sql-transaction (forall (r) ((struct sql-connection) (-> r) -> r)))
(define (within-sql-transaction sql-connection procedure)
  (sql-execute sql-connection "BEGIN IMMEDIATE TRANSACTION;" (list))
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
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (let ((sqlite3-step-result (sqlite3-step sqlite3-stmt*)))
          (if (= sqlite3-step-result sqlite3-result-busy)
            (sql-raise-deadlock-exception statement))
          (if (not (= sqlite3-step-result sqlite3-result-done))
            (abort
              (format "failed to execute statement ~A with error code ~A"
                statement
                sqlite3-step-result))))))))

;; executes a sql statement that returns rows
(: sql-read ((struct sql-connection) string list -> (list-of (list-of *))))
(define (sql-read sql-connection statement parameter-values)
  (let ((sqlite3* (sql-connection-sqlite3* sql-connection)))
    (with-sqlite3-stmt* sqlite3* statement parameter-values
      (lambda (sqlite3-stmt*)
        (sql-read-all-rows sqlite3-stmt*)))))

;; returns the id generated for the last inserted row
(: sql-last-generated-id ((struct sql-connection) -> fixnum))
(define (sql-last-generated-id sql-connection)
  (caar (sql-read sql-connection "SELECT last_insert_rowid();" (list))))

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
                    (debug-print ((condition-property-accessor 'sql-deadlock 'statement) exception))
                    (abort exception))
                  (begin
                    (debug-print (format "DEADLOCK occured on try ~A" inner-count))
                    (debug-print ((condition-property-accessor 'sql-deadlock 'statement) exception))
                    (with-sql-retry-on-deadlock-inner (+ inner-count 1))))
                (abort exception)))
            (procedure)))))
    (with-sql-retry-on-deadlock-inner 1)))

;; returns a string to be used for accent and case
;; insensitive searches using the like value% construct
(: sql-searchable-string (string -> string))
(define (sql-searchable-string string)
  (utf8-lower-case
    (utf8-remove-accents
      (string-delete
        (string->char-set "%_")
        string))))
