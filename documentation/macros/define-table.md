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
Expands to the following procedures.  
The insert procedure returns the generated id.  
The select procedure returns a list containing the matching row.

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

select procedures
-----------------
Each select procedure expands to a procedure such as below,  
which returns a list containing the matching rows.

    (define (employees-table-select-by-name
      sql-connection
      name))

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
    (define-table ...)

    (with-sql-connection "example.db"
      (lambda (sql-connection)

        (display
          (employees-table-insert
            sql-connection
            (make-employee-row
              #f
              "Alice"
              (make-date 2008 4 23))))
        (newline)

        (display
          (employees-table-select-by-employee-id
            sql-connection
            1))
        (newline)

        (employees-table-update
          sql-connection
          (make-employee-row
            1000
            "Alice"
            (make-date 2008 9 25)))))

Run the following commands.

    $ make
    $ ./main

    1
    (#<employee-row>)
