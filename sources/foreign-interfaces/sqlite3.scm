(declare (unit sqlite3))

(foreign-declare "

#include <sqlite3.h>

// allocates a sqlite3*
sqlite3** malloc_sqlite3_pointer()
{
  sqlite3** sqlite3_pointer = sqlite3_malloc(sizeof(sqlite3*));
  return sqlite3_pointer;
}

// resolves a sqlite3*
sqlite3* resolve_sqlite3_pointer(sqlite3** sqlite3_pointer)
{
  return *sqlite3_pointer;
}

// frees a sqlite3*
void free_sqlite3_pointer(sqlite3** sqlite3_pointer)
{
  sqlite3_free(sqlite3_pointer);
}

// allocates a sqlite3_stmt*
sqlite3_stmt** malloc_sqlite3_stmt_pointer()
{
  sqlite3_stmt** sqlite3_stmt_pointer = sqlite3_malloc(sizeof(sqlite3_stmt*));
  return sqlite3_stmt_pointer;
}

// resolves a sqlite3_stmt*
sqlite3_stmt* resolve_sqlite3_stmt_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  return *sqlite3_stmt_pointer;
}

// frees a sqlite3_stmt*
void free_sqlite3_stmt_pointer(sqlite3_stmt** sqlite3_stmt_pointer)
{
  sqlite3_free(sqlite3_stmt_pointer);
}

")

;; result codes
(define sqlite3-result-ok (foreign-value "SQLITE_OK" int))
(define sqlite3-result-row (foreign-value "SQLITE_ROW" int))
(define sqlite3-result-done (foreign-value "SQLITE_DONE" int))
(define sqlite3-result-busy (foreign-value "SQLITE_BUSY" int))

;; sqlite3 pointers definitions
(define-foreign-type sqlite3 "sqlite3")
(define-foreign-type sqlite3* (c-pointer sqlite3))
(define-foreign-type sqlite3** (c-pointer sqlite3*))

;; sqlite3 pointers memory management
(define malloc-sqlite3* (foreign-lambda sqlite3** "malloc_sqlite3_pointer"))
(define resolve-sqlite3* (foreign-lambda sqlite3* "resolve_sqlite3_pointer" sqlite3**))
(define free-sqlite3* (foreign-lambda void "free_sqlite3_pointer" sqlite3**))

;; sqlite3-stmt pointers definitions
(define-foreign-type sqlite3-stmt "sqlite3_stmt")
(define-foreign-type sqlite3-stmt* (c-pointer sqlite3-stmt))
(define-foreign-type sqlite3-stmt** (c-pointer sqlite3-stmt*))

;; sqlite3-stmt pointers memory management
(define malloc-sqlite3-stmt*
  (foreign-lambda sqlite3-stmt** "malloc_sqlite3_stmt_pointer"))
(define resolve-sqlite3-stmt*
  (foreign-lambda sqlite3-stmt* "resolve_sqlite3_stmt_pointer" sqlite3-stmt**))
(define free-sqlite3-stmt*
  (foreign-lambda void "free_sqlite3_stmt_pointer" sqlite3-stmt**))

;; opens a database connection
(define sqlite3-open (foreign-lambda int "sqlite3_open" (const c-string) sqlite3**))

;; compiles a sql statement
(define sqlite3-prepare-v2
  (foreign-lambda int "sqlite3_prepare_v2"
    sqlite3* (const c-string) int sqlite3-stmt** (const (c-pointer c-string))))

;; special binding behaviors
(define sqlite3-static (foreign-value "SQLITE_STATIC" (function void (c-pointer))))
(define sqlite3-transient (foreign-value "SQLITE_TRANSIENT" (function void (c-pointer))))

;; binding values to prepared statements
(define sqlite3-bind-int
  (foreign-lambda int "sqlite3_bind_int" sqlite3-stmt* int int))
(define sqlite3-bind-double
  (foreign-lambda int "sqlite3_bind_double" sqlite3-stmt* int double))
(define sqlite3-bind-text
  (foreign-lambda int "sqlite3_bind_text"
    sqlite3-stmt* int (const c-string) int (function void (c-pointer))))
(define sqlite3-bind-null
  (foreign-lambda int "sqlite3_bind_null" sqlite3-stmt* int))

;; evaluates a sql statement
(define sqlite3-step (foreign-lambda int "sqlite3_step" sqlite3-stmt*))

;; number of columns in a result set
(define sqlite3-column-count (foreign-lambda int "sqlite3_column_count" sqlite3-stmt*))

;; column datatypes
(define sqlite3-type-integer (foreign-value "SQLITE_INTEGER" int))
(define sqlite3-type-float (foreign-value "SQLITE_FLOAT" int))
(define sqlite3-type-text (foreign-value "SQLITE_TEXT" int))
(define sqlite3-type-null (foreign-value "SQLITE_NULL" int))

;; result values from a query
(define sqlite3-column-type
  (foreign-lambda int "sqlite3_column_type" sqlite3-stmt* int))
(define sqlite3-column-int
  (foreign-lambda int "sqlite3_column_int" sqlite3-stmt* int))
(define sqlite3-column-double
  (foreign-lambda double "sqlite3_column_double" sqlite3-stmt* int))
(define sqlite3-column-text
  (foreign-lambda unsigned-c-string "sqlite3_column_text" sqlite3-stmt* int))

;; destroy a prepared statement object
(define sqlite3-finalize (foreign-lambda int "sqlite3_finalize" sqlite3-stmt*))

;; closes a database connection
(define sqlite3-close-v2 (foreign-lambda int "sqlite3_close_v2" sqlite3*))
