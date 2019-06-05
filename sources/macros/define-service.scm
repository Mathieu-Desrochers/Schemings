(define-syntax validate-request
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let ((request-symbol (cadr exp))
            (validate-request-procedure-symbol (caddr exp)))

        ;; validate the request
        `(let ((validation-errors (,validate-request-procedure-symbol ,request-symbol)))
           (unless (null? validation-errors)
             (raise-validation-errors-exception validation-errors)))))))

(define-syntax select-one
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((row-expression (list-ref exp 1))
             (row-symbol (list-ref row-expression 0))
             (table-select-by-symbol (list-ref row-expression 1))
             (table-select-by-parameter-symbols (drop row-expression 2))
             (body (drop exp 2)))

        ;; select the rows
        `(let ((,(rename 'rows)
                (,table-select-by-symbol
                  sql-connection
                  ,@table-select-by-parameter-symbols)))

          ;; bring the selected row into scope
          (let ((,row-symbol
                  (if (not (null? ,(rename 'rows)))
                    (car ,(rename 'rows))
                    #f)))

            ;; execute the body
            ,(if (not (null? body))
              `(begin ,@body)
              #f)))))))

(define-syntax select-one-and-validate
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((row-expression (list-ref exp 1))
             (row-symbol (list-ref row-expression 0))
             (table-select-by-symbol (list-ref row-expression 1))
             (table-select-by-parameter-symbols (drop row-expression 2))
             (validation-error-expression (list-ref exp 2))
             (validation-error-symbol (list-ref validation-error-expression 0))
             (body (drop exp 3)))

        ;; select the rows
        `(let ((,(rename 'rows)
                (,table-select-by-symbol
                  sql-connection
                  ,@table-select-by-parameter-symbols)))

          ;; validate a row was selected
          (when (null? ,(rename 'rows))
            (raise-validation-errors-exception
              (list (quote ,validation-error-symbol))))

          ;; bring the selected row into scope
          (let ((,row-symbol (car ,(rename 'rows))))

            ;; execute the body
            ,(if (not (null? body))
              `(begin ,@body)
              #f)))))))

(define-syntax select-many
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((rows-expression (list-ref exp 1))
             (rows-symbol (list-ref rows-expression 0))
             (table-select-by-symbol (list-ref rows-expression 1))
             (table-select-by-parameter-symbols (drop rows-expression 2))
             (body (drop exp 2)))

        ;; select the rows and
        ;; bring them into scope
        `(let ((,rows-symbol
                (,table-select-by-symbol
                  sql-connection
                  ,@table-select-by-parameter-symbols)))

          ;; execute the body
          ,(if (not (null? body))
            `(begin ,@body)
            #f))))))

(define-syntax select-many-and-hash-by-unique-key
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((rows-expression (list-ref exp 1))
             (rows-symbol (list-ref rows-expression 0))
             (table-select-by-symbol (list-ref rows-expression 1))
             (table-select-by-parameter-symbols (drop rows-expression 2))
             (hash-table-expression (list-ref exp 2))
             (hash-table-ref-symbol (list-ref hash-table-expression 0))
             (hash-table-key-symbol (list-ref hash-table-expression 1))
             (body (drop exp 3)))

        ;; select the rows and
        ;; bring them into scope
        `(let ((,rows-symbol
                (,table-select-by-symbol
                  sql-connection
                  ,@table-select-by-parameter-symbols)))

          ;; make the hash table
          (let ((,(rename 'hash-table)
                 (hash-by-unique-key
                   ,rows-symbol
                   ,hash-table-key-symbol
                   identity)))

            ;; define the ref procedure and
            ;; bring it into scope
            (let ((,hash-table-ref-symbol
                    (lambda (key)
                      (hash-table-ref/default
                        ,(rename 'hash-table)
                        key
                        #f))))

              ;; execute the body
              ,(if (not (null? body))
                `(begin ,@body)
                #f))))))))

(define-syntax select-many-and-hash-by-shared-key
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((rows-expression (list-ref exp 1))
             (rows-symbol (list-ref rows-expression 0))
             (table-select-by-symbol (list-ref rows-expression 1))
             (table-select-by-parameter-symbols (drop rows-expression 2))
             (hash-table-expression (list-ref exp 2))
             (hash-table-ref-symbol (list-ref hash-table-expression 0))
             (hash-table-key-symbol (list-ref hash-table-expression 1))
             (body (drop exp 3)))

        ;; select the rows and
        ;; bring them into scope
        `(let ((,rows-symbol
                (,table-select-by-symbol
                  sql-connection
                  ,@table-select-by-parameter-symbols)))

          ;; make the hash table
          (let ((,(rename 'hash-table)
                 (hash-by-shared-key
                   ,rows-symbol
                   ,hash-table-key-symbol
                   identity)))

            ;; define the ref procedure and
            ;; bring it into scope
            (let ((,hash-table-ref-symbol
                    (lambda (key)
                      (hash-table-ref/default
                        ,(rename 'hash-table)
                        key
                        (list)))))

              ;; execute the body
              ,(if (not (null? body))
                `(begin ,@body)
                #f))))))))

(define-syntax validate-duplicates
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (list-ref exp 1))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-value-symbol (list-ref subrequests-expression 2))
             (validation-errors-expression (list-ref exp 2))
             (validation-errors-prefix-symbol (list-ref validation-errors-expression 0))
             (validation-errors-suffix-symbol (list-ref validation-errors-expression 1)))

        ;; get the duplicates index
        `(let ((,(rename 'duplicates-index)
                (list-duplicates-index
                  (,request-value-symbol ,request-symbol)
                  ,subrequest-value-symbol)))

          ;; validate there are no duplicates index
          (unless (null? ,(rename 'duplicates-index))
            (raise-validation-errors-exception
              (map
                (lambda (duplicates-index)
                  (symbol-append
                    (quote ,validation-errors-prefix-symbol) '-
                    (string->symbol (number->string duplicates-index)) '-
                    (quote ,validation-errors-suffix-symbol)))
                ,(rename 'duplicates-index)))))))))

(define-syntax validate-references
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (list-ref exp 1))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-value-symbol (list-ref subrequests-expression 2))
             (rows-expression (list-ref exp 2))
             (rows-symbol (list-ref rows-expression 0))
             (row-value-symbol (list-ref rows-expression 1))
             (validation-errors-expression (list-ref exp 3))
             (validation-errors-prefix-symbol (list-ref validation-errors-expression 0))
             (validation-errors-suffix-symbol (list-ref validation-errors-expression 1)))

        ;; get the non-matches index
        `(let ((,(rename 'non-matches-index)
                (list-non-matches-index
                  (,request-value-symbol ,request-symbol)
                  ,subrequest-value-symbol
                  ,rows-symbol
                  ,row-value-symbol)))

          ;; validate there are no non-matches index
          (unless (null? ,(rename 'non-matches-index))
            (raise-validation-errors-exception
              (map
                (lambda (non-match-index)
                  (symbol-append
                    (quote ,validation-errors-prefix-symbol) '-
                    (string->symbol (number->string non-match-index)) '-
                    (quote ,validation-errors-suffix-symbol)))
                ,(rename 'non-matches-index)))))))))

(define-syntax validate-inserted-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (list-ref exp 1))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-value-symbol (list-ref subrequests-expression 2))
             (validation-errors-expression (list-ref exp 2))
             (validation-errors-prefix-symbol (list-ref validation-errors-expression 0))
             (validation-errors-suffix-symbol (list-ref validation-errors-expression 1))
             (validation-procedure (list-ref exp 3)))

        ;; validate the subrequests
        `(let ((,(rename 'invalid-subrequests-index)
                (list-filtered-index
                  (,request-value-symbol ,request-symbol)
                  identity
                  (lambda (,(rename 'subrequest))

                    ;; only subrequests without an id are inserted
                    ;; the other ones cannot be invalid
                    (if (not (,subrequest-value-symbol ,(rename 'subrequest)))
                      (not (,validation-procedure ,(rename 'subrequest)))
                      #f)))))

          ;; validate there are no invalid subrequests index
          (unless (null? ,(rename 'invalid-subrequests-index))
            (raise-validation-errors-exception
              (map
                (lambda (invalid-subrequest-index)
                  (symbol-append
                    (quote ,validation-errors-prefix-symbol) '-
                    (string->symbol (number->string invalid-subrequest-index)) '-
                    (quote ,validation-errors-suffix-symbol)))
                ,(rename 'invalid-subrequests-index)))))))))

(define-syntax validate-updated-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (list-ref exp 1))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-value-symbol (list-ref subrequests-expression 2))
             (rows-expression (list-ref exp 2))
             (rows-symbol (list-ref rows-expression 0))
             (row-value-symbol (list-ref rows-expression 1))
             (validation-errors-expression (list-ref exp 3))
             (validation-errors-prefix-symbol (list-ref validation-errors-expression 0))
             (validation-errors-suffix-symbol (list-ref validation-errors-expression 1))
             (validation-procedure (list-ref exp 4)))

        ;; hash the rows
        `(let ((,(rename 'rows-hash-table)
                 (hash-by-unique-key
                   ,rows-symbol
                   ,row-value-symbol
                   identity)))

          ;; validate the subrequests
          (let ((,(rename 'invalid-subrequests-index)
                  (list-filtered-index
                    (,request-value-symbol ,request-symbol)
                    identity
                    (lambda (,(rename 'subrequest))

                      ;; only subrequests with an id are updated
                      ;; the other ones cannot be invalid
                      (if (,subrequest-value-symbol ,(rename 'subrequest))

                        ;; get the matching row
                        (let ((,(rename 'row)
                                (hash-table-ref/default
                                  ,(rename 'rows-hash-table)
                                  (,subrequest-value-symbol ,(rename 'subrequest))
                                  #f)))

                          ;; only subrequests with a matching row are updated
                          ;; the other ones cannot be invalid
                          (if ,(rename 'row)
                            (not (,validation-procedure ,(rename 'subrequest) ,(rename 'row)))
                            #f))

                        #f)))))

            ;; validate there are no invalid subrequests index
            (unless (null? ,(rename 'invalid-subrequests-index))
              (raise-validation-errors-exception
                (map
                  (lambda (invalid-subrequest-index)
                    (symbol-append
                      (quote ,validation-errors-prefix-symbol) '-
                      (string->symbol (number->string invalid-subrequest-index)) '-
                      (quote ,validation-errors-suffix-symbol)))
                  ,(rename 'invalid-subrequests-index))))))))))

(define-syntax validate-deleted-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (list-ref exp 1))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-value-symbol (list-ref subrequests-expression 2))
             (rows-expression (list-ref exp 2))
             (rows-symbol (list-ref rows-expression 0))
             (row-value-symbol (list-ref rows-expression 1))
             (validation-error-expression (list-ref exp 3))
             (validation-error-symbol (list-ref validation-error-expression 0))
             (validation-procedure (list-ref exp 4)))

        ;; hash the subrequests
        `(let ((,(rename 'subrequests-hash-table)
                 (hash-by-unique-key
                   (filter ,subrequest-value-symbol (,request-value-symbol ,request-symbol))
                   ,subrequest-value-symbol
                   identity)))

          ;; validate the rows
          (let ((,(rename 'invalid-rows-index)
                  (list-filtered-index
                    ,rows-symbol
                    identity
                    (lambda (,(rename 'row))

                      ;; get the matching subrequest
                      (let ((,(rename 'subrequest)
                              (hash-table-ref/default
                                ,(rename 'subrequests-hash-table)
                                (,row-value-symbol ,(rename 'row))
                                #f)))

                        ;; only rows with no matching subrequest are deleted
                        ;; the other ones cannot be invalid
                        (if (not ,(rename 'subrequest))
                          (not (,validation-procedure ,(rename 'row)))
                          #f))))))

            ;; validate there are no invalid rows index
            (unless (null? ,(rename 'invalid-rows-index))
              (raise-validation-errors-exception
                (list ',validation-error-symbol)))))))))

(define-syntax update-modified-rows
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((subrequests-expression (list-ref exp 1))
             (request-value-symbol (list-ref subrequests-expression 0))
             (request-symbol (list-ref subrequests-expression 1))
             (subrequest-id-symbol (list-ref subrequests-expression 2))
             (subrequest-value-symbols (drop subrequests-expression 3))
             (rows-expression (list-ref exp 2))
             (rows-symbol (list-ref rows-expression 0))
             (row-id-symbol (list-ref rows-expression 1))
             (row-value-symbols (drop rows-expression 2))
             (table-expression (list-ref exp 3))
             (table-symbol (list-ref table-expression 0))
             (make-inserted-row-procedure (list-ref exp 4))
             (make-updated-row-procedure (list-ref exp 5)))

        ;; compare the elements
        `(let ((compare-results
                (compare-elements

                  ;; the original rows
                  ,rows-symbol
                  ,row-id-symbol

                  ;; the current subrequests
                  (,request-value-symbol ,request-symbol)
                  ,subrequest-id-symbol

                  ;; returns whether an element has changed
                  (lambda (row subrequest)
                    (not
                      (equal?
                        (list
                          ,@(map
                            (lambda (row-value-symbol)
                              `(,row-value-symbol row))
                            row-value-symbols))
                        (list
                          ,@(map
                            (lambda (subrequest-value-symbol)
                              `(,subrequest-value-symbol subrequest))
                            subrequest-value-symbols)))))

                  ;; makes an added row
                  ,make-inserted-row-procedure

                  ;; makes a changed row
                  ,make-updated-row-procedure

                  ;; makes an unchanged row
                  (lambda (row subrequest) row)

                  ;; makes a deleted row
                  identity)))

          ;; insert the added rows
          (for-each
            (lambda (row)
              (,(symbol-append table-symbol '-insert)
                sql-connection
                row))
            (compare-results-added-elements
              compare-results))

          ;; update the changed rows
          (for-each
            (lambda (row)
              (,(symbol-append table-symbol '-update)
                sql-connection
                row))
            (compare-results-changed-elements
              compare-results))

          ;; delete the deleted rows
          (for-each
            (lambda (row)
              (,(symbol-append table-symbol '-delete)
                sql-connection
                row))
            (compare-results-deleted-elements
              compare-results)))))))

(define-syntax make-subresponses
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; parses the expression
      (let* ((rows-expression (list-ref exp 1))
             (rows-symbol (list-ref rows-expression 0))
             (sort-procedure (list-ref rows-expression 1))
             (sort-value-procedure (list-ref rows-expression 2))
             (make-subresponse-procedure (list-ref exp 2)))

        ;; map the make-subresponse-procedure to every row
        `(map
          ,make-subresponse-procedure

          ;; sort the rows to provide a predictable
          ;; and repeatable ordering of the subresponses
          (,sort-procedure
            ,rows-symbol
            ,sort-value-procedure))))))
