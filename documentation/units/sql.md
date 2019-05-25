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
insensitive searches using the like value% construct.

__string__  
A string.

__result__  
The searchable string.

try it
------
Place the following code in sources/main.scm.

    (declare (uses sql))


Run the following commands.

    $ make
    $ ./main


powered by
----------
The great sqlite3.
