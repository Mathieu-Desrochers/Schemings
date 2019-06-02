define-table
------------
Defines a table.

    (define-table
      (employees-table
        "employees")
      (employee-row
        ("employee-id" integer)
        ("name" string)
        ("hiring-date" date))
      (select-procedures
        (employees-table-select-by-name
          (string-append
            "SELECT * "
            "FROM \"employees\" "
            "WHERE \"name\" = ?1;")
          ("?1" string)))
      (execute-procedures
        (employees-table-delete-by-name
          (string-append
            "DELETE "
            "FROM \"employees\" "
            "WHERE \"name\" = ?1;")
          ("?1" string))))

Supported column types (sqlite3 type):

- boolean (INTEGER)
- integer (INTEGER)
- number (REAL)
- string (TEXT)
- date (TEXT)
- date-time (TEXT)
- time (TEXT)

Assumes the first column is declared like so.

- INTEGER PRIMARY KEY AUTOINCREMENT

record definition
-----------------
Expands to a record definition.

    (define-record employee-row
      employee-id
      name
      hiring-date)

basic procedures
----------------
Expands to the following procedures:

    (define (employees-table-insert
      sql-connection
      employee-row))

    (define (employees-table-select-by-employee-id
      sql-connection
      employee-id))

    (define (employees-table-update
      sql-connection
      employee-row))

    (define (employees-table-delete
      sql-connection
      employee-row))

__results__  
The insert procedure returns the generated id.

    1000

The select procedure returns a list containing the matching row.

    (list #<employee-row>)

select procedures
-----------------
Each select procedure expands to a procedure such as:

    (define (employees-table-select-by-name
      sql-connection
      name))

__result__  
A list containing the matching rows.

    (list
      #<employee-row>
      #<employee-row>
      ...)

execute procedures
------------------
Each execute procedure expands to a procedure such as:

    (define (employees-table-delete-by-name
      sql-connection
      name))

try it
------
Run the following commands.

    $ sqlite3 example.db

    sqlite> CREATE TABLE "employees"
       ...> (
       ...>   "employee-id" INTEGER PRIMARY KEY AUTOINCREMENT,
       ...>   "name" TEXT,
       ...>   "hiring-date" TEXT
       ...> );
    sqlite> .exit

Place the following code in sources/main.scm.

    (import srfi-1)

    (declare (uses sql))

    ;; as above
    (define-table
      ...)

    (with-sql-connection "example.db"
      (lambda (sql-connection)

        (let ((alice-id
                (employees-table-insert
                  sql-connection
                  (make-employee-row
                    #f
                    "Alice"
                    (make-date 2008 4 23)))))
          (display alice-id)
          (newline)

          (let ((alice-row
                  (car
                    (employees-table-select-by-employee-id
                      sql-connection
                      alice-id))))
            (employee-row-hiring-date-set! alice-row (make-date 2008 4 25))
            (employees-table-update
              sql-connection
              alice-row)))))
