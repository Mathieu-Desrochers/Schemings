(import-for-syntax srfi-1)

(define-syntax define-request
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; returns the scheme type of a field
      (define (field-type->scheme-type field-type-info)
        (let* ((field-type (list-ref field-type-info 0))
               (field-required (or (eq? field-type 'boolean) (list-ref field-type-info 1))))
          (if field-required
            (cond ((eq? field-type 'blob) `blob)
                  ((eq? field-type 'boolean) `boolean)
                  ((eq? field-type 'integer) `fixnum)
                  ((eq? field-type 'number) `number)
                  ((eq? field-type 'string) `string)
                  ((eq? field-type 'date) `(struct date))
                  ((eq? field-type 'date-time) `(struct date-time))
                  ((eq? field-type 'time) `(struct time))
                  ((eq? field-type 'list)
                    `(list-of ,(field-type->scheme-type (drop field-type-info 4))))
                  ((string-contains (symbol->string field-type) "subrequest")
                    `(struct ,field-type)))
            (cond ((eq? field-type 'blob) `(or blob false))
                  ((eq? field-type 'integer) `(or fixnum false))
                  ((eq? field-type 'number) `(or number false))
                  ((eq? field-type 'string) `(or string false))
                  ((eq? field-type 'date) `(or (struct date) false))
                  ((eq? field-type 'date-time) `(or (struct date-time) false))
                  ((eq? field-type 'time) `(or (struct time) false))
                  ((eq? field-type 'list)
                    `(or (list-of ,(field-type->scheme-type (drop field-type-info 4))) false))
                  ((string-contains (symbol->string field-type) "subrequest")
                    `(or (struct ,field-type) false))))))

      ;; validates a value field
      (define (validate-value-field request-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field))
              (field-validation-parameters (cddr field)))
          `(let ((validation-error
                  (,(symbol-append 'validate- field-type)
                    (,(symbol-append request-symbol '- field-symbol) ,request-symbol)
                    ,@field-validation-parameters)))
            (if validation-error
              (list (symbol-append ',field-symbol '- validation-error))
              (list #f)))))

      ;; validates a value list field
      (define (validate-value-list-field request-symbol field)
        (let* ((field-list-symbol (list-ref field 0))
               (field-list-validation-parameters (take (drop field 2) 3))
               (field-element-type (list-ref field 5))
               (field-element-validation-parameters (drop field 6)))
          `(let ((validation-error
                  (validate-list
                    (,(symbol-append request-symbol '- field-list-symbol) ,request-symbol)
                    ,@field-list-validation-parameters)))
            (if validation-error
              (list (symbol-append ',field-list-symbol '- validation-error))
              (map
                (lambda (element-index)
                  (let* ((validation-error
                          (,(symbol-append 'validate- field-element-type)
                            (list-ref
                              (,(symbol-append request-symbol '- field-list-symbol) ,request-symbol)
                              element-index)
                            ,@field-element-validation-parameters)))
                    (if validation-error
                      (symbol-append
                        ',field-list-symbol '-
                        (string->symbol (number->string element-index)) '-
                        validation-error)
                      #f)))
                (iota (length (,(symbol-append request-symbol '- field-list-symbol) ,request-symbol))))))))

      ;; validates a subrequest field
      (define (validate-subrequest-field request-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field))
              (field-validation-parameters (cddr field)))
          `(let ((validation-error
                  (validate-record
                    (,(symbol-append request-symbol '- field-symbol) ,request-symbol)
                    ,@field-validation-parameters)))
            (if validation-error
              (list (symbol-append ',field-symbol '- validation-error))
              (let ((validation-errors
                      (,(symbol-append 'validate- field-type)
                        (,(symbol-append request-symbol '- field-symbol) ,request-symbol))))
                (map
                  (lambda (validation-error)
                    (symbol-append ',field-symbol '- validation-error))
                  validation-errors))))))

      ;; validates a subrequest list field
      (define (validate-subrequest-list-field request-symbol field)
        (let* ((field-list-symbol (list-ref field 0))
               (field-list-validation-parameters (take (drop field 2) 3))
               (field-element-type (list-ref field 5))
               (field-element-validation-parameters (drop field 6)))
          `(let ((validation-error
                  (validate-list
                    (,(symbol-append request-symbol '- field-list-symbol) ,request-symbol)
                    ,@field-list-validation-parameters)))
            (if validation-error
              (list (symbol-append ',field-list-symbol '- validation-error))
              (append-map
                (lambda (element-index)
                  (let ((validation-error
                          (validate-record
                            (list-ref
                              (,(symbol-append request-symbol '- field-list-symbol) ,request-symbol)
                              element-index)
                            ,@field-element-validation-parameters)))
                    (if validation-error
                      (list
                        (symbol-append
                          ',field-list-symbol '-
                          (string->symbol (number->string element-index)) '-
                          validation-error))
                      (let ((validation-errors
                              (,(symbol-append 'validate- field-element-type)
                                (list-ref
                                  (,(symbol-append request-symbol '- field-list-symbol) ,request-symbol)
                                  element-index))))
                        (map
                          (lambda (validation-error)
                            (symbol-append
                              ',field-list-symbol '-
                              (string->symbol (number->string element-index)) '-
                              validation-error))
                          validation-errors)))))
                (iota (length (,(symbol-append request-symbol '- field-list-symbol) ,request-symbol))))))))

      ;; parses a value field from a json node
      (define (json-parse-value-field request-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field)))
          `(let ((value (json-object-get-value json-node ,(symbol->string field-symbol))))
             (json-value->field-value value ',field-type))))

      ;; parses a value list field from a json node
      (define (json-parse-value-list-field request-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field)))
          `(let ((json-array-node
                  (json-object-get-array
                    json-node
                    ,(symbol->string field-symbol))))
            (if json-array-node
              (map
                (lambda (index)
                  (let ((value (json-array-get-value json-array-node index)))
                    (json-value->field-value value ',field-type)))
                (iota (json-array-length json-array-node)))
              #f))))

      ;; parses a subrequest field from a json node
      (define (json-parse-subrequest-field request-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field)))
          `(let ((json-object-node
                  (json-object-get-object
                    json-node
                    ,(symbol->string field-symbol))))
            (if json-object-node
              (,(symbol-append 'json-parse- field-type)
                json-object-node)
              #f))))

      ;; parses a subrequest list field from a json node
      (define (json-parse-subrequest-list-field request-symbol field)
        (let* ((field-symbol (list-ref field 0))
               (field-element-type (list-ref field 5)))
          `(let ((json-array-node
                  (json-object-get-array
                    json-node
                    ,(symbol->string field-symbol))))
            (if json-array-node
              (map
                (lambda (index)
                  (let ((json-object-node
                          (json-array-get-object
                            json-array-node
                            index)))
                    (if json-object-node
                      (,(symbol-append 'json-parse- field-element-type)
                        json-object-node)
                      #f)))
                (iota (json-array-length json-array-node)))
              #f))))

      ;; parses the expression
      (let* ((request-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          (import srfi-1)

          (declare (uses validation))

          ;; encapsulates a request
          (define-record ,request-symbol ,@fields-symbol)

          ;; fields are untyped before validation
          (: ,(symbol-append 'make- request-symbol) (
            ,@(make-list (length fields) `*) -> (struct ,request-symbol)))
          ,@(map
            (lambda (field)
              `(: ,(symbol-append request-symbol '- (car field)) (
                (struct ,request-symbol) -> *)))
            fields)
          ,@(map
            (lambda (field)
              `(: ,(symbol-append request-symbol '- (car field) '-set!) (
                (struct ,request-symbol) * -> noreturn)))
            fields)

          ;; once validation has passed
          ;; fields are known to be of the right type
          ;; and should be accessed by these getters
          ,@(concatenate
            (map
              (lambda (field)
                (list
                  `(: ,(symbol-append request-symbol '- (car field) '*) (
                    (struct ,request-symbol) -> ,(field-type->scheme-type (drop field 1))))
                  `(define (,(symbol-append request-symbol '- (car field) '*) ,request-symbol)
                     (,(symbol-append request-symbol '- (car field)) ,request-symbol))))
              fields))

          ;; validates a request
          (: ,(symbol-append 'validate- request-symbol) (
            (struct ,request-symbol) -> (list-of symbol)))
          (define (,(symbol-append 'validate- request-symbol) ,request-symbol)
            (filter
              (lambda (validation-error) validation-error)
              (append
                ,@(map
                  (lambda (field)
                    (let ((field-type (cadr field)))
                      (cond ((eq? field-type 'list)
                              (if (string-contains (symbol->string (list-ref field 5)) "subrequest")
                                (validate-subrequest-list-field request-symbol field)
                                (validate-value-list-field request-symbol field)))
                            ((string-contains (symbol->string field-type) "subrequest")
                              (validate-subrequest-field request-symbol field))
                            (else
                              (validate-value-field request-symbol field)))))
                  fields))))

          ;; parses a request from a json node
          (: ,(symbol-append 'json-parse- request-symbol) (
            (struct json-node) -> (struct ,request-symbol)))
          (define (,(symbol-append 'json-parse- request-symbol) json-node)
            (,(symbol-append 'make- request-symbol)
              ,@(map
                (lambda (field)
                  (let ((field-type (cadr field)))
                    (cond ((eq? field-type 'list)
                            (if (string-contains (symbol->string (list-ref field 5)) "subrequest")
                              (json-parse-subrequest-list-field request-symbol field)
                              (json-parse-value-list-field request-symbol field)))
                          ((string-contains (symbol->string field-type) "subrequest")
                            (json-parse-subrequest-field request-symbol field))
                          (else
                            (json-parse-value-field request-symbol field)))))
                fields)))

          ;; parses a request from a json string
          (: ,(symbol-append 'try-json-string-> request-symbol) (
            string -> (struct ,request-symbol)))
          (define (,(symbol-append 'try-json-string-> request-symbol) json-string)
            (with-string->json json-string
              (lambda (json-node)
                (if json-node
                  (,(symbol-append 'json-parse- request-symbol) json-node)
                  #f)))))))))
