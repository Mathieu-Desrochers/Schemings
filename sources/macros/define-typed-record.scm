(define-syntax define-typed-record
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; encapsulates a field
      (define (make-field symbol type) (list symbol type))
      (define (field-symbol field) (car field))
      (define (field-type field) (cadr field))

      ;; makes fields based on their definition
      (define (make-fields fields-definition)
        (map
          (lambda (field-definition)
            (make-field
              (car field-definition)
              (cadr field-definition)))
        fields-definition))

      ;; parses the expression
      (let* ((record-symbol (cadr exp))
             (fields (make-fields (cddr exp))))
        `(begin

          ;; record definition
          (define-record ,record-symbol ,@(map field-symbol fields))

          ;; make procedure
          (: ,(symbol-append 'make- record-symbol) (
            ,@(map field-type fields) -> (struct ,record-symbol)))

          ;; get procedures
          ,@(map
            (lambda (field)
              `(: ,(symbol-append record-symbol '- (field-symbol field)) (
                (struct ,record-symbol) -> ,(field-type field))))
            fields)

          ;; set procedures
          ,@(map
            (lambda (field)
              `(: ,(symbol-append record-symbol '- (field-symbol field) '-set!) (
                (struct ,record-symbol) ,(field-type field) -> noreturn)))
            fields))))))
