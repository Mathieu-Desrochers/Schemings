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
            fields)

          ;; make copy procedure
          (: ,(symbol-append 'make- record-symbol '-copy) (
             (struct ,record-symbol) -> (struct ,record-symbol)))
          (define (,(symbol-append 'make- record-symbol '-copy) source)
            (,(symbol-append 'make- record-symbol)
              ,@(map
                (lambda (field)
                  `(,(symbol-append record-symbol '- (car field)) source))
                fields))))))))

(define-syntax make-record-copy
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; encapsulates an assignation
      (define (make-assignation field-symbol expression) (list field-symbol expression))
      (define (assignation-field-symbol assignation) (car assignation))
      (define (assignation-expression assignation) (cadr assignation))

      ;; parses the expression
      (let* ((record-expression (cadr exp))
             (record-type (car record-expression))
             (record-symbol (cadr record-expression))
             (assignations-list (cddr exp))
             (assignations
                (map
                  (lambda (assignation-pair)
                    (make-assignation
                      (car assignation-pair)
                      (cadr assignation-pair)))
                  assignations-list)))
        `(begin
          (let ((,(rename 'copy) (,(symbol-append 'make- record-type '-copy) ,record-symbol)))
            ,@(map
              (lambda (assignation)
                `(,(symbol-append record-type '- (assignation-field-symbol assignation) '-set!)
                   ,(rename 'copy)
                   ,(assignation-expression assignation)))
              assignations)
            ,(rename 'copy)))))))
