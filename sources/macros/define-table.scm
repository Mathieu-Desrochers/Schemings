(import-for-syntax srfi-1)
(import-for-syntax srfi-13)

(define-syntax define-table
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; converts a table type to a scheme type
      (define (table-type->scheme-type table-type)
        (cond ((eq? table-type 'boolean) `boolean)
              ((eq? table-type 'integer) `(or fixnum false))
              ((eq? table-type 'number) `(or number false))
              ((eq? table-type 'string) `(or string false))
              ((eq? table-type 'date) `(or (struct date) false))
              ((eq? table-type 'date-time) `(or (struct date-time) false))
              ((eq? table-type 'time) `(or (struct time) false))))

      ;; encapsulates a column
      (define (make-column name symbol type) (list name symbol type))
      (define (column-name column) (car column))
      (define (column-symbol column) (cadr column))
      (define (column-type column) (caddr column))
      (define (column-scheme-type column)
        (table-type->scheme-type (column-type column)))

      ;; makes columns based on their definition
      (define (make-columns columns-definition)
        (map
          (lambda (column-definition)
            (make-column
              (car column-definition)
              (string->symbol (car column-definition))
              (cadr column-definition)))
        columns-definition))

      ;; joins a set of columns name
      (define (join-columns-name columns)
        (string-join
          (map
            (lambda (column)
              (string-append "\"" (column-name column) "\""))
            columns)
          ", "))

      ;; joins a set of columns variable
      (define (join-columns-variable columns)
        (string-join
          (map
            (lambda (column-index)
              (string-append "$" (number->string (+ column-index 1))))
            (iota (length columns)))
          ", "))

      ;; joins a set of columns assignation
      (define (join-columns-assignation columns first-variable-number)
        (string-join
          (map
            (lambda (column-index)
              (string-append
                "\"" (column-name (list-ref columns column-index)) "\" = "
                "$" (number->string (+ column-index first-variable-number))))
            (iota (length columns)))
          ", "))

      ;; encapsulates a parameter
      (define (make-parameter name symbol type) (list name symbol type))
      (define (parameter-name parameter) (car parameter))
      (define (parameter-symbol parameter) (cadr parameter))
      (define (parameter-type parameter) (caddr parameter))
      (define (parameter-scheme-type parameter)
        (table-type->scheme-type (parameter-type parameter)))

      ;; makes parameters based on their definition
      (define (make-parameters parameters-definition)
        (map
          (lambda (parameter-definition)
            (make-parameter
              (car parameter-definition)
              (string->symbol (car parameter-definition))
              (cadr parameter-definition)))
        parameters-definition))

      ;; parses the expression
      (let* ((table-expression (list-ref exp 1))
             (table-symbol (list-ref table-expression 0))
             (table-name (list-ref table-expression 1))
             (row-expression (list-ref exp 2))
             (row-symbol (car row-expression))
             (columns (make-columns (cdr row-expression)))
             (id-column (car columns))
             (value-columns (cdr columns))
             (select-procedures (cdr (list-ref exp 3)))
             (execute-procedures (cdr (list-ref exp 4))))
        `(begin

          (import srfi-1)

          (declare (uses monitoring))
          (declare (uses sql))

          ;; encapsulates a row
          (define-typed-record ,row-symbol
            ,@(map
                (lambda (column)
                  `(,(column-symbol column) ,(column-scheme-type column)))
                columns))

          ;; inserts a row
          (: ,(symbol-append table-symbol '-insert) (
            (struct sql-connection) (struct ,row-symbol) -> fixnum))
          (define (,(symbol-append table-symbol '-insert) sql-connection ,row-symbol)
            (with-monitoring-timing
              ,(string-append
                 "tables,name=" (symbol->string table-symbol) ","
                 "procedure=" (symbol->string table-symbol) "-insert")
              (lambda ()
                (sql-execute
                  sql-connection
                  ,(string-append
                    "INSERT INTO \"" table-name "\" (" (join-columns-name value-columns) ") "
                    "VALUES (" (join-columns-variable value-columns) ");")
                  (list
                    ,@(map
                      (lambda (column)
                        `(table-value->sql-value
                          (,(symbol-append row-symbol '- (column-symbol column)) ,row-symbol)
                          ',(column-type column)))
                      value-columns)))
                (sql-last-generated-id sql-connection))))

          ;; selects a row by id
          (: ,(symbol-append table-symbol '-select-by- (column-symbol id-column)) (
            (struct sql-connection) ,(column-scheme-type id-column) ->
            (list-of (struct ,row-symbol))))
          (define (,(symbol-append table-symbol '-select-by- (column-symbol id-column))
                    sql-connection ,(column-symbol id-column))
            (with-monitoring-timing
              ,(string-append
                 "tables,name=" (symbol->string table-symbol) ","
                 "procedure=" (symbol->string table-symbol) "-select-by-"
                 (symbol->string (column-symbol id-column)))
              (lambda ()
                (map
                  (lambda (row)
                    (apply
                      ,(symbol-append 'make- row-symbol)
                      (map
                        (lambda (zipped-value)
                          (sql-value->table-value (car zipped-value) (cadr zipped-value)))
                        (zip row (list ,@(map (lambda (column) `',(column-type column)) columns))))))
                  (sql-read
                    sql-connection
                    ,(string-append
                      "SELECT * "
                      "FROM \"" table-name "\" "
                      "WHERE \"" (column-name id-column) "\" = $1;")
                    (list
                      ,(column-symbol id-column)))))))

          ;; selects all the rows
          (: ,(symbol-append table-symbol '-select-all) (
            (struct sql-connection) -> (list-of (struct ,row-symbol))))
          (define (,(symbol-append table-symbol '-select-all) sql-connection)
            (with-monitoring-timing
              ,(string-append
                 "tables,name=" (symbol->string table-symbol) ","
                 "procedure=" (symbol->string table-symbol) "-select-all")
              (lambda ()
                (map
                  (lambda (row)
                    (apply
                      ,(symbol-append 'make- row-symbol)
                      (map
                        (lambda (zipped-value)
                          (sql-value->table-value (car zipped-value) (cadr zipped-value)))
                        (zip row (list ,@(map (lambda (column) `',(column-type column)) columns))))))
                  (sql-read
                    sql-connection
                    ,(string-append
                      "SELECT * "
                      "FROM \"" table-name "\";")
                    (list))))))

          ;; updates a row
          (: ,(symbol-append table-symbol '-update) (
            (struct sql-connection) (struct ,row-symbol) -> noreturn))
          (define (,(symbol-append table-symbol '-update) sql-connection ,row-symbol)
            (with-monitoring-timing
              ,(string-append
                 "tables,name=" (symbol->string table-symbol) ","
                 "procedure=" (symbol->string table-symbol) "-update")
              (lambda ()
                (sql-execute
                  sql-connection
                  ,(string-append
                    "UPDATE \"" table-name "\" "
                    "SET " (join-columns-assignation value-columns 2)
                    "WHERE \"" (column-name id-column) "\" = $1;")
                  (list
                    ,@(map
                      (lambda (column)
                        `(table-value->sql-value
                          (,(symbol-append row-symbol '- (column-symbol column)) ,row-symbol)
                          ',(column-type column)))
                      columns))))))

          ;; deletes a row
          (: ,(symbol-append table-symbol '-delete) (
            (struct sql-connection) (struct ,row-symbol) -> noreturn))
          (define (,(symbol-append table-symbol '-delete) sql-connection ,row-symbol)
            (with-monitoring-timing
              ,(string-append
                 "tables,name=" (symbol->string table-symbol) ","
                 "procedure=" (symbol->string table-symbol) "-delete")
              (lambda ()
                (sql-execute
                  sql-connection
                  ,(string-append
                    "DELETE "
                    "FROM \"" table-name "\" "
                    "WHERE \"" (column-name id-column) "\" = $1;")
                  (list
                    (,(symbol-append row-symbol '- (column-symbol id-column)) ,row-symbol))))))

          ;; select procedures
          ,@(concatenate
            (map
              (lambda (select-procedure)
                (let ((select-procedure-symbol (car select-procedure))
                      (select-procedure-sql (cadr select-procedure))
                      (select-procedure-parameters (make-parameters (cddr select-procedure))))
                  (list
                    `(: ,select-procedure-symbol (
                      (struct sql-connection)
                      ,@(map parameter-scheme-type select-procedure-parameters) ->
                      (list-of (struct ,row-symbol))))
                    `(define (,select-procedure-symbol sql-connection
                               ,@(map parameter-symbol select-procedure-parameters))
                      (with-monitoring-timing
                        ,(string-append
                           "tables,name=" (symbol->string table-symbol) ","
                           "procedure=" (symbol->string select-procedure-symbol))
                        (lambda ()
                          (map
                            (lambda (row)
                              (apply
                                ,(symbol-append 'make- row-symbol)
                                (map
                                  (lambda (zipped-value)
                                    (sql-value->table-value (car zipped-value) (cadr zipped-value)))
                                  (zip row (list ,@(map (lambda (column) `',(column-type column)) columns))))))
                            (sql-read
                              sql-connection
                              ,select-procedure-sql
                              (list
                                ,@(map
                                  (lambda (parameter)
                                    `(table-value->sql-value
                                      ,(parameter-symbol parameter)
                                      ',(parameter-type parameter)))
                                  select-procedure-parameters))))))))))
              select-procedures))

          ;; execute procedures
          ,@(concatenate
            (map
              (lambda (execute-procedure)
                (let ((execute-procedure-symbol (car execute-procedure))
                      (execute-procedure-sql (cadr execute-procedure))
                      (execute-procedure-parameters (make-parameters (cddr execute-procedure))))
                  (list
                    `(: ,execute-procedure-symbol (
                      (struct sql-connection)
                      ,@(map parameter-scheme-type execute-procedure-parameters) ->
                      noreturn))
                    `(define (,execute-procedure-symbol sql-connection
                               ,@(map parameter-symbol execute-procedure-parameters))
                      (with-monitoring-timing
                        ,(string-append
                           "tables,name=" (symbol->string table-symbol) ","
                           "procedure=" (symbol->string execute-procedure-symbol))
                        (lambda ()
                          (sql-execute
                            sql-connection
                            ,execute-procedure-sql
                            (list
                              ,@(map
                                (lambda (parameter)
                                  `(table-value->sql-value
                                    ,(parameter-symbol parameter)
                                    ',(parameter-type parameter)))
                                execute-procedure-parameters)))))))))
              execute-procedures)))))))
