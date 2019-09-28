(import-for-syntax scheme)

(define-syntax define-response
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; returns the scheme type of a field
      (define (field-type->scheme-type field-type-info)
        (let ((field-type (list-ref field-type-info 0)))
          (cond ((eq? field-type 'blob) `(or blob false))
                ((eq? field-type 'boolean) `boolean)
                ((eq? field-type 'integer) `(or fixnum false))
                ((eq? field-type 'number) `(or number false))
                ((eq? field-type 'string) `(or string false))
                ((eq? field-type 'date) `(or (struct date) false))
                ((eq? field-type 'date-time) `(or (struct date-time) false))
                ((eq? field-type 'time) `(or (struct time) false))
                ((eq? field-type 'list)
                  `(or (list-of ,(field-type->scheme-type (drop field-type-info 1))) false))
                ((string-contains (symbol->string field-type) "subresponse")
                  `(or (struct ,field-type) false)))))

      ;; formats a value field to a json node
      (define (json-format-value-field response-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field)))
          `(let ((value (,(symbol-append response-symbol '- field-symbol) ,response-symbol)))
            (json-object-add-value
              json-node
              ,(symbol->string field-symbol)
              (field-value->json-value value ',field-type)))))

      ;; formats a value list field to a json node
      (define (json-format-value-list-field response-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field)))
          `(let ((value-list (,(symbol-append response-symbol '- field-symbol) ,response-symbol)))
            (if value-list
              (let ((json-array-node
                      (json-object-add-array
                        json-node
                        ,(symbol->string field-symbol))))
                (for-each
                  (lambda (value)
                    (json-array-add-value
                      json-array-node
                      (field-value->json-value value ',field-type)))
                  value-list))
              (json-object-add-value
                json-node
                ,(symbol->string field-symbol)
                'json-null)))))

      ;; formats a subresponse field to a json node
      (define (json-format-subresponse-field response-symbol field)
        (let ((field-symbol (car field))
              (field-type (cadr field)))
          `(let ((subresponse (,(symbol-append response-symbol '- field-symbol) ,response-symbol)))
            (if subresponse
              (let ((json-object-node
                      (json-object-add-object
                        json-node
                        ,(symbol->string field-symbol))))
                (,(symbol-append 'json-format- field-type)
                  subresponse
                  json-object-node))
              (json-object-add-value
                json-node
                ,(symbol->string field-symbol)
                'json-null)))))

      ;; formats a subresponse list field to a json node
      (define (json-format-subresponse-list-field response-symbol field)
        (let* ((field-symbol (car field))
               (field-type (caddr field)))
          `(let ((subresponse-list (,(symbol-append response-symbol '- field-symbol) ,response-symbol)))
            (if subresponse-list
              (let ((json-array-node
                      (json-object-add-array
                        json-node
                        ,(symbol->string field-symbol))))
                (for-each
                  (lambda (subresponse)
                    (if subresponse
                      (let ((json-object-node
                              (json-array-add-object
                                json-array-node)))
                        (,(symbol-append 'json-format- field-type)
                          subresponse
                          json-object-node))
                      (json-array-add-value
                        json-array-node
                        'json-null)))
                  subresponse-list))
              (json-object-add-value
                json-node
                ,(symbol->string field-symbol)
                'json-null)))))

      ;; parses the expression
      (let* ((response-symbol (cadr exp))
             (fields (cddr exp))
             (fields-symbol (map car fields)))
        `(begin

          (import srfi-1)

          ;; encapsulates a response
          (define-record ,response-symbol ,@fields-symbol)

          ;; declare the response types
          (: ,(symbol-append 'make- response-symbol) (
            ,@(map (lambda (field) (field-type->scheme-type (drop field 1))) fields) ->
            (struct ,response-symbol)))
          ,@(map
            (lambda (field)
              `(: ,(symbol-append response-symbol '- (car field)) (
                (struct ,response-symbol) -> ,(field-type->scheme-type (drop field 1)))))
            fields)
          ,@(map
            (lambda (field)
              `(: ,(symbol-append response-symbol '- (car field) '-set!) (
                (struct ,response-symbol) ,(field-type->scheme-type (drop field 1)) -> noreturn)))
            fields)

          ;; formats a response to a json node
          (: ,(symbol-append 'json-format- response-symbol) (
            (struct ,response-symbol) (struct json-node) -> noreturn))
          (define (,(symbol-append 'json-format- response-symbol) ,response-symbol json-node)
            (begin
              ,@(map
                (lambda (field)
                  (let ((field-type (cadr field)))
                    (cond ((and
                              (eq? field-type 'list)
                              (string-contains (symbol->string (list-ref field 2)) "subresponse"))
                            (json-format-subresponse-list-field response-symbol field))
                          ((string-contains (symbol->string field-type) "subresponse")
                            (json-format-subresponse-field response-symbol field))
                          ((eq? field-type 'list)
                            (json-format-value-list-field response-symbol field))
                          (else
                            (json-format-value-field response-symbol field)))))
                fields)))

          ;; formats a response to a json string
          (: ,(symbol-append response-symbol '->json-string) (
            (struct ,response-symbol) -> string))
          (define (,(symbol-append response-symbol '->json-string) ,response-symbol)
            (with-json-object
              (lambda (json-node)
                (,(symbol-append 'json-format- response-symbol) ,response-symbol json-node)
                (json->string json-node)))))))))
