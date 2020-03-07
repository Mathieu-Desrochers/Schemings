with-sql-connection
-------------------
Invokes a procedure with a sql connection.

__database-name__  
A path to a sqlite3 database file.

__procedure__  
A procedure invoked with the sql connection.

within-sql-transaction
----------------------
Executes a procedure within a transaction.  
If no exception is raised, the transaction is automatically committed.  
Otherwise it is automatically rollbacked.

__sql-connection__  
A sql connection.

__procedure__  
A procedure invoked within the transaction.

sql-execute
-----------
Executes a sql statement.

__sql-connection__  
A sql connection.

__statement__  
A sql statement possibly containing parameters.

__parameter-values__  
A list of values for the statement parameters.

sql-read
--------
Executes a sql statement that return rows.

__sql-connection__  
A sql connection.

__statement__  
A sql statement possibly containing parameters.

__parameter-values__  
A list of values for the statement parameters.

__result__  
The list of rows represented as lists of values.

sql-last-generated-id
---------------------
Returns the id generated for the last inserted row.

__sql-connection__  
A sql connection.

__result__  
The generated id.

sql-searchable-string
---------------------
Returns a string to be used for accent and case  
insensitive searches using the LIKE value% construct.

__string__  
A string.

__result__  
The searchable string.

with-sql-deadlock-retries
-------------------------
Executes a procedure until no deadlock occurs  
or a maximum number of retries is reached.

__count__  
A maximum number of retries.

__procedure__  
A procedure to invoke.

sql-raise-deadlock-exception
----------------------------
Raises a deadlock exception.

sql-deadlock-exception?
-----------------------
Returns whether an exception was caused by a deadlock.

__exception__  
An exception.

without-sql-transaction
-----------------------
Executes a procedure outside of the current transaction.  
The transaction is committed then restarted.

__sql-connection__  
A sql connection.

__procedure__  
A procedure invoked without the transaction.

try it
------
Run the following commands.

    $ sqlite3 example.db

    sqlite> CREATE TABLE "employees"
       ...> (
       ...>   "employee-id" INTEGER PRIMARY KEY AUTOINCREMENT,
       ...>   "name" TEXT,
       ...>   "salary" INTEGER
       ...> );
    sqlite> .exit

Place the following code in sources/main.scm.

    (declare (uses sql))

    (with-sql-connection "example.db"
      (lambda (sql-connection)

        (sql-execute
          sql-connection
          "INSERT INTO employees (name, salary) VALUES (?1, ?2);"
          (list "Alice" 24000))

        (sql-execute
          sql-connection
          "INSERT INTO employees (name, salary) VALUES (?1, ?2);"
          (list "Bob" 27000))

        (display
          (sql-read
            sql-connection
            "SELECT * FROM employees WHERE salary > ?1;"
            (list 20000)))

        (newline)))

Run the following commands.

    $ make
    $ ./main

    ((1 Alice 24000) (2 Bob 27000))

powered by
----------
The great sqlite3.
