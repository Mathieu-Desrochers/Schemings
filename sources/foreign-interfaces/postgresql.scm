(declare (unit postgresql))

(foreign-declare "

#include <libpq-fe.h>
#include <postgresql/server/catalog/pg_type_d.h>

char** postgresql_make_params_array(int size)
{
  return calloc(size, sizeof(char*));
}

void postgresql_set_params_array(char** array, int index, char* value)
{
  if (value == NULL)
    return;

  array[index] = malloc((strlen(value) * sizeof(char)) + 1);
  strcpy(array[index], value);
}

void postgresql_free_params_array(char** array, int size)
{
  for (int index = 0; index < size; index++)
    free(array[index]);

  free(array);
}

")

;; result codes
(define pqstatus-connection-ok (foreign-value "CONNECTION_OK" int))
(define pgres-command-ok (foreign-value "PGRES_COMMAND_OK" int))
(define pgres-tuples-ok (foreign-value "PGRES_TUPLES_OK" int))

;; pgconn pointers definitions
(define-foreign-type pgconn "PGconn")
(define-foreign-type pgconn* (c-pointer pgconn))

;; pgresult pointers definitions
(define-foreign-type pgresult "PGresult")
(define-foreign-type pgresult* (c-pointer pgresult))

;; parameters memory management
(define postgresql-make-params-array (foreign-lambda c-pointer "postgresql_make_params_array" int))
(define postgresql-set-params-array (foreign-lambda void "postgresql_set_params_array" c-pointer int c-string))
(define postgresql-free-params-array (foreign-lambda void "postgresql_free_params_array" c-pointer int))

;; makes a new connection to the database server
(define pqconnectdb (foreign-lambda pgconn* "PQconnectdb" (const c-string)))

;; returns the status of the connection
(define pqstatus (foreign-lambda int "PQstatus" pgconn*))
(define pqerrormessage (foreign-lambda c-string "PQerrorMessage" pgconn*))

;; submits a command to the server and waits for the result
(define pqexecparams (foreign-lambda pgresult* "PQexecParams"
  pgconn* (const c-string) int u32vector c-pointer u32vector u32vector int))

;; returns the result status of the command
(define pqresultstatus (foreign-lambda int "PQresultStatus" pgresult*))
(define pqresulterrormessage (foreign-lambda c-string "PQresultErrorMessage" pgresult*))

;; dimensions of a result set
(define pqntuples (foreign-lambda int "PQntuples" pgresult*))
(define pqnfields (foreign-lambda int "PQnfields" pgresult*))

;; column datatypes
(define postgresql-type-bool (foreign-value "BOOLOID" int))
(define postgresql-type-int4 (foreign-value "INT4OID" int))
(define postgresql-type-numeric (foreign-value "NUMERICOID" int))
(define postgresql-type-float8 (foreign-value "FLOAT8OID" int))
(define postgresql-type-char (foreign-value "CHAROID" int))
(define postgresql-type-text (foreign-value "TEXTOID" int))
(define postgresql-type-date (foreign-value "DATEOID" int))
(define postgresql-type-timestamp (foreign-value "TIMESTAMPOID" int))
(define postgresql-type-time (foreign-value "TIMEOID" int))

;; result values from a query
(define pqftype (foreign-lambda int "PQftype" pgresult* int))
(define pqgetvalue (foreign-lambda c-string "PQgetvalue" pgresult* int int))

;; frees the storage associated with a pgresult
(define pqclear (foreign-lambda void "PQclear" pgresult*))

;; closes the connection to the server
(define pqfinish (foreign-lambda void "PQfinish" pgconn*))
