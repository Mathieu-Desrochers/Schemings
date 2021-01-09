define-service
--------------
A service can be any procedure, but  
using this macro provides free features.

    (define-service ([service-name] ...)
      ...)

If the monitoring unit is initialized, the execution  
time of the service procedure will be tracked.

validate-request
----------------
Validates a request.

    (validate-request [request] [validation-procedure])

Invokes a validation procedure on a request.  
Raises a validation errors exception if it fails.

select-one
----------
Selects one row.

    (select-one
      ([select-procedure]
        [select-parameter-1]
        [select-parameter-2]
        ...))

Invokes a select procedure and returns the matching row.  
Returns false if no row was returned.

select-one-and-validate
-----------------------
Selects one row and validates it exists.

    (select-one-and-validate
      ([select-procedure]
        [select-parameter-1]
        [select-parameter-2]
        ...)
      ([validation-error-symbol]))

Invokes a select procedure and returns the matching row.  
Raises a validation errors exception if no row was returned.

select-many
-----------
Selects many rows.

    (select-many
      ([select-procedure]
        [select-parameter-1]
        [select-parameter-2]
        ...))

Invokes a select procedure and returns the matching rows.

select-many-and-hash-by-unique-key
----------------------------------
Selects many rows and hashes them by a unique key.

    (select-many-and-hash-by-unique-key
      ([select-procedure]
        [select-parameter-1]
        [select-parameter-2]
        ...)
      ([row-value-get-procedure]))

Returns a procedure to access the matching rows by key.  
This procedure returns a single element or false.

select-many-and-hash-by-shared-key
----------------------------------
Selects many rows and hashes them by a shared key.

    (select-many-and-hash-by-shared-key
      ([select-procedure]
        [select-parameter-1]
        [select-parameter-2]
        ...)
      ([row-value-get-procedure]))

Returns a procedure to access the matching rows by key.  
This procedure returns a list of elements.

validate-duplicates
-------------------
Validates the subrequest values are not duplicated.

    (validate-duplicates
      ([request-subrequests-get-procedure]
        [request-symbol]
        [subrequest-value-get-procedure])
      ([validation-error-symbol-prefix]
        [validation-error-symbol-suffix]))

Raised validation-errors are built from the values index  
between the specified prefix and suffix.

validate-references
-------------------
Validates the subrequest values are present in the row values.  
This is done by joining them on the specified values.

    (validate-references
      ([request-subrequests-get-procedure]
        [request-symbol]
        [subrequest-value-get-procedure])
      ([rows-symbol]
        [row-value-get-procedure])
      ([validation-error-symbol-prefix]
        [validation-error-symbol-suffix]))

Raised validation-errors are built from the values index  
between the specified prefix and suffix.

validate-inserted-rows
----------------------
Validates the subrequests for which the specified property is false.  
The predicate must return whether the insertion is allowed.

    (validate-inserted-rows
      ([request-subrequests-get-procedure]
        [request-symbol]
        [subrequest-value-get-procedure])
      ([validation-error-symbol-prefix]
        [validation-error-symbol-suffix])

      (lambda ([subrequest-symbol])
        [predicate]))

Raised validation-errors are built from the values index  
between the specified prefix and suffix.

validate-updated-rows
---------------------
Validates the subrequests and rows that match on the specified values.  
The predicate must return whether the update is allowed.

    (validate-updated-rows
      ([request-subrequests-get-procedure]
        [request-symbol]
        [subrequest-value-get-procedure])
      ([rows-symbol]
        [row-value-get-procedure])
      ([validation-error-symbol-prefix]
        [validation-error-symbol-suffix])

      (lambda ([subrequest-symbol] [row-symbol])
        [predicate]))

Raised validation-errors are built from the values index  
between the specified prefix and suffix.

validate-deleted-rows
---------------------
Validates the rows that cannot be matched to a subrequest.  
The predicate must return whether the deletion is allowed.

    (validate-deleted-rows
      ([request-subrequests-get-procedure]
        [request-symbol]
        [subrequest-value-get-procedure])
      ([rows-symbol]
        [row-value-get-procedure])
      ([validation-error-symbol])

      (lambda ([row-symbol])
        [predicate]))

Raised validation-errors are built from the values index  
between the specified prefix and suffix.

update-modified-rows
--------------------
Matches the subrequests and rows on the first specified values.

Subrequests that have not matched are passed to the first procedure.  
Matching pairs are compared on the other specified values.  
If anything has changed they are passed to the second procedure.  
Rows that have no match are deleted.

The first procedure must return a row that will be inserted.  
The second one must return a row that will be updated.

    (update-modified-rows
      ([request-subrequests-get-procedure]
        [request-symbol]
        [subrequest-value-get-procedure-1]
        [subrequest-value-get-procedure-2]
        ...)
      ([rows-symbol]
        [row-value-get-procedure-1]
        [row-value-get-procedure-2]
        ...)
      ([table-symbol])

      (lambda ([subrequest-symbol])
        [make-inserted-row-body])

      (lambda ([row-symbol] [subrequest-symbol])
        [make-updated-row-body]))

update-modified-rows-and-subrows
--------------------------------
Same as update-modified-rows but two levels deep.

    (update-modified-rows-and-subrows
      ([request-subrequests-get-procedure]
        [request-symbol]
        [subrequest-value-get-procedure-1]
        [subrequest-value-get-procedure-2]
        ...)
      ([rows-symbol]
        [row-value-get-procedure-1]
        [row-value-get-procedure-2]
        ...)
      ([table-symbol])
      ...

The first procedure is invoked to get each subrequest's children.  
The second one serves the same purpose for rows.

      ([subrequest-subsubrequests-get-procedure]
        [subsubrequest-value-get-procedure-1]
        [subsubrequest-value-get-procedure-2]
        ...)
      ([sub-rows-ref-procedure]
        [subrow-value-get-procedure-1]
        [subrow-value-get-procedure-2]
        ...)
      ...

The procedure is invoked to delete all subrows matching a row id.

      ([subtable-symbol]
        [subtable-delete-all-children-procedure])
      ...

These procedures are the same as in update-modified-rows.

      (lambda ([subrequest-symbol])
        [make-inserted-row-body])

      (lambda ([row-symbol] [subrequest-symbol])
        [make-updated-row-body]))
      ...

The first procedure must return a subrow that will be inserted.  
The second one must return a subrow that will be updated.

      (lambda ([subsubrequest-symbol row-id])
        [make-inserted-subrow-body])

      (lambda ([subrow-symbol] [subsubrequest-symbol])
        [make-updated-subrow-body]))

make-subresponses
-----------------
Sorts a list of rows and maps them to a procedure  
which returns their matching subresponses.

    (make-subresponses
      ([rows-symbol]
        sort-by-number
        [row-value-get-procedure])
      (lambda ([row-symbol])
        [make-subresponse-body]))

try it
------
Run the following commands.

    $ sqlite3 cookies.db

    sqlite> CREATE TABLE "cookies"
       ...> (
       ...>   "cookie-id" INTEGER PRIMARY KEY AUTOINCREMENT,
       ...>   "name" TEXT
       ...> );

    sqlite> INSERT INTO "cookies" ("name") VALUES ("chocolate");

    sqlite> CREATE TABLE "ingredients"
       ...> (
       ...>   "ingredient-id" INTEGER PRIMARY KEY AUTOINCREMENT,
       ...>   "name" TEXT,
       ...>   "calories" INTEGER
       ...> );

    sqlite> INSERT INTO "ingredients" ("name", "calories") VALUES ("chocolate", 100);
    sqlite> INSERT INTO "ingredients" ("name", "calories") VALUES ("milk", 50);
    sqlite> INSERT INTO "ingredients" ("name", "calories") VALUES ("butter", 200);
    sqlite> INSERT INTO "ingredients" ("name", "calories") VALUES ("flour", 20);

    sqlite> CREATE TABLE "cookie-ingredients"
       ...> (
       ...>   "cookie-ingredient-id" INTEGER PRIMARY KEY AUTOINCREMENT,
       ...>   "cookie-id" INTEGER,
       ...>   "ingredient-id" INTEGER,
       ...>   "quantity" INTEGER,
       ...>   FOREIGN KEY ("cookie-id") REFERENCES "cookies" ("cookie-id"),
       ...>   FOREIGN KEY ("ingredient-id") REFERENCES "ingredients" ("ingredient-id")
       ...> );

    sqlite> INSERT INTO "cookie-ingredients" ("cookie-id", "ingredient-id", "quantity") VALUES (1, 1, 5);
    sqlite> INSERT INTO "cookie-ingredients" ("cookie-id", "ingredient-id", "quantity") VALUES (1, 2, 2);
    sqlite> INSERT INTO "cookie-ingredients" ("cookie-id", "ingredient-id", "quantity") VALUES (1, 3, 1);

    sqlite> .exit

Place the following code in sources/main.scm.

    (import srfi-1)
    (import srfi-69)

    (import (chicken condition))

    (declare (uses compare))
    (declare (uses hash))
    (declare (uses list))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; tables definition

    (define-table
      (cookies-table
        "cookies")
      (cookie-row
        ("cookie-id" integer)
        ("name" string))
      (select-procedures)
      (execute-procedures))

    (define-table
      (ingredients-table
        "ingredients")
      (ingredient-row
        ("ingredient-id" integer)
        ("name" string)
        ("calories" integer))
      (select-procedures)
      (execute-procedures))

    (define-table
      (cookie-ingredients-table
        "cookie-ingredients")
      (cookie-ingredient-row
        ("cookie-ingredient-id" integer)
        ("cookie-id" integer)
        ("ingredient-id" integer)
        ("quantity" integer))
      (select-procedures
        (cookie-ingredients-table-select-by-cookie-id
          (string-append
            "SELECT * "
            "FROM \"cookie-ingredients\" "
            "WHERE \"cookie-id\" = ?1;")
          ("?1" integer)))
      (execute-procedures))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; request definition

    (define-request update-cookie-request
      (cookie-id integer #t 1 999999)
      (name string #t 1 100)
      (cookie-ingredients list #t 1 10 update-cookie-cookie-ingredient-subrequest #t))

    (define-request update-cookie-cookie-ingredient-subrequest
      (cookie-ingredient-id integer #f 1 999999)
      (ingredient-id integer #t 1 999999)
      (quantity integer #t 1 999999))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; response definition

    (define-response update-cookie-response)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; service definition

    (: update-cookie-service (
      (struct update-cookie-request)
      (struct sql-connection) ->
      (struct update-cookie-response)))

    (define-service
      (update-cookie-service
        update-cookie-request
        sql-connection)

      ;; validate the request
      (validate-request
        update-cookie-request
        validate-update-cookie-request)

      ;; validate there are no ingredient-id duplicates
      (validate-duplicates
        (update-cookie-request-cookie-ingredients*
          update-cookie-request
          update-cookie-cookie-ingredient-subrequest-ingredient-id*)
        (cookie-ingredients
          ingredient-id-duplicate))

      (let (

          ;; select and validate the cookie-row
          (cookie-row
            (select-one-and-validate
              (cookies-table-select-by-cookie-id
                (update-cookie-request-cookie-id*
                  update-cookie-request))
              (unknown-cookie-id)))

          ;; select the cookie-ingredient-rows
          (cookie-ingredient-rows
            (select-many
              (cookie-ingredients-table-select-by-cookie-id
                (update-cookie-request-cookie-id*
                  update-cookie-request)))))

        ;; validate the referred cookie-ingredient-ids
        (validate-references
          (update-cookie-request-cookie-ingredients*
            update-cookie-request
            update-cookie-cookie-ingredient-subrequest-cookie-ingredient-id*)
          (cookie-ingredient-rows
            cookie-ingredient-row-cookie-ingredient-id)
          (cookie-cookie-ingredients
            cookie-ingredient-id-unknown))

        (let (

            ;; select the ingredient-rows
            (ingredient-rows
              (select-many
                (ingredients-table-select-all))))

          ;; validate the referred ingredient-ids
          (validate-references
            (update-cookie-request-cookie-ingredients*
              update-cookie-request
              update-cookie-cookie-ingredient-subrequest-ingredient-id*)
            (ingredient-rows
              ingredient-row-ingredient-id)
            (cookie-ingredients
              ingredient-id-unknown))

          ;; validate the inserted cookie-ingredient-rows
          (validate-inserted-rows
            (update-cookie-request-cookie-ingredients*
              update-cookie-request
              update-cookie-cookie-ingredient-subrequest-cookie-ingredient-id*)
            (cookie-ingredients
              quantity-too-low)

            ;; always use plenty of stuff
            (lambda (update-cookie-cookie-ingredient-subrequest)
              (>=
                (update-cookie-cookie-ingredient-subrequest-quantity
                  update-cookie-cookie-ingredient-subrequest)
                10)))

          ;; update the cookie-row
          (cookies-table-update
            sql-connection
            (make-cookie-row
              (update-cookie-request-cookie-id* update-cookie-request)
              (update-cookie-request-name* update-cookie-request)))

          ;; update the modified cookie-ingredient-rows
          (update-modified-rows
            (update-cookie-request-cookie-ingredients*
              update-cookie-request
              update-cookie-cookie-ingredient-subrequest-cookie-ingredient-id*
              update-cookie-cookie-ingredient-subrequest-ingredient-id*
              update-cookie-cookie-ingredient-subrequest-quantity*)
            (cookie-ingredient-rows
              cookie-ingredient-row-cookie-ingredient-id
              cookie-ingredient-row-ingredient-id
              cookie-ingredient-row-quantity)
            (cookie-ingredients-table)

            ;; makes a new cookie-ingredient-row
            (lambda (cookie-ingredient-subrequest)
              (make-cookie-ingredient-row
                #f
                (update-cookie-request-cookie-id*
                  update-cookie-request)
                (update-cookie-cookie-ingredient-subrequest-ingredient-id*
                  cookie-ingredient-subrequest)
                (update-cookie-cookie-ingredient-subrequest-quantity*
                  cookie-ingredient-subrequest)))

            ;; makes an updated cookie-ingredient-row
            (lambda (cookie-ingredient-row cookie-ingredient-subrequest)
              (make-record-copy
                (cookie-ingredient-row cookie-ingredient-row)
                (ingredient-id
                  (update-cookie-cookie-ingredient-subrequest-ingredient-id*
                    cookie-ingredient-subrequest))
                (quantity
                  (update-cookie-cookie-ingredient-subrequest-quantity*
                    cookie-ingredient-subrequest)))))))

      ;; make the update-cookie-response
      (make-update-cookie-response))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; service invocation

    ;; display validation errors
    (handle-exceptions
      exception
      (if (validation-errors-exception? exception)
        (let ((validation-errors (validation-errors-exception-validation-errors exception)))
          (display validation-errors)
          (newline))
        (abort exception))

      ;; connect to the database
      (with-sql-connection
        "cookies.db"
        (lambda (sql-connection)

          ;; invoke the service
          (update-cookie-service
            (make-update-cookie-request
              1
              "double-chocolate"
              (list
                (make-update-cookie-cookie-ingredient-subrequest 1 1 10)
                (make-update-cookie-cookie-ingredient-subrequest 2 2 2)
                (make-update-cookie-cookie-ingredient-subrequest 3 3 1)
                (make-update-cookie-cookie-ingredient-subrequest #f 4 12)))

          sql-connection))))

Run the following commands.

    $ make
    $ ./main
