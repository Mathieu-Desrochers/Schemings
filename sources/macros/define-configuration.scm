(import-for-syntax srfi-1)
(import-for-syntax srfi-13)

(define-syntax define-configuration
  (er-macro-transformer
    (lambda (exp rename compare)

      ;; returns the scheme type of a setting
      (define (setting-type->scheme-type setting-type-info)
        (let* ((setting-type (list-ref setting-type-info 0))
               (setting-required (or (eq? setting-type 'boolean) (list-ref setting-type-info 1))))
          (if setting-required
            (cond ((eq? setting-type 'boolean) `boolean)
                  ((eq? setting-type 'integer) `fixnum)
                  ((eq? setting-type 'number) `number)
                  ((eq? setting-type 'string) `string)
                  ((eq? setting-type 'list)
                    `(list-of ,(setting-type->scheme-type (drop setting-type-info 4))))
                  ((string-contains (symbol->string setting-type) "subconfiguration")
                    `(struct ,setting-type)))
            (cond ((eq? setting-type 'integer) `(or fixnum false))
                  ((eq? setting-type 'number) `(or number false))
                  ((eq? setting-type 'string) `(or string false))
                  ((eq? setting-type 'list)
                    `(or (list-of ,(setting-type->scheme-type (drop setting-type-info 4))) false))
                  ((string-contains (symbol->string setting-type) "subconfiguration")
                    `(or (struct ,setting-type) false))))))

      ;; validates a value setting
      (define (validate-value-setting configuration-symbol setting)
        (let* ((setting-symbol (car setting))
               (setting-type (cadr setting))
               (setting-has-default-value
                  (any (lambda (symbol) (eq? symbol 'default)) (cdr setting)))
               (setting-validation-parameters
                  (if setting-has-default-value
                    (drop-right (cddr setting) 2)
                    (cddr setting))))
          `(let ((validation-error
                  (,(symbol-append 'validate- setting-type)
                    (,(symbol-append configuration-symbol '- setting-symbol) ,configuration-symbol)
                    ,@setting-validation-parameters)))
            (if validation-error
              (list (symbol-append ',setting-symbol '- validation-error))
              (list #f)))))

      ;; validates a value list setting
      (define (validate-value-list-setting configuration-symbol setting)
        (let* ((setting-list-symbol (list-ref setting 0))
               (setting-list-validation-parameters (take (drop setting 2) 3))
               (setting-element-type (list-ref setting 5))
               (setting-element-validation-parameters (drop setting 6)))
          `(let ((validation-error
                  (validate-list
                    (,(symbol-append configuration-symbol '- setting-list-symbol) ,configuration-symbol)
                    ,@setting-list-validation-parameters)))
            (if validation-error
              (list (symbol-append ',setting-list-symbol '- validation-error))
              (map
                (lambda (element-index)
                  (let* ((validation-error
                          (,(symbol-append 'validate- setting-element-type)
                            (list-ref
                              (,(symbol-append configuration-symbol '- setting-list-symbol)
                                ,configuration-symbol)
                              element-index)
                            ,@setting-element-validation-parameters)))
                    (if validation-error
                      (symbol-append
                        ',setting-list-symbol '-
                        (string->symbol (number->string element-index)) '-
                        validation-error)
                      #f)))
                (iota
                  (length
                    (,(symbol-append configuration-symbol '- setting-list-symbol)
                      ,configuration-symbol))))))))

      ;; validates a subconfiguration setting
      (define (validate-subconfiguration-setting configuration-symbol setting)
        (let ((setting-symbol (car setting))
              (setting-type (cadr setting))
              (setting-validation-parameters (cddr setting)))
          `(let ((validation-error
                  (validate-record
                    (,(symbol-append configuration-symbol '- setting-symbol) ,configuration-symbol)
                    ,@setting-validation-parameters)))
            (if validation-error
              (list (symbol-append ',setting-symbol '- validation-error))
              (let ((validation-errors
                      (,(symbol-append 'validate- setting-type)
                        (,(symbol-append configuration-symbol '- setting-symbol) ,configuration-symbol))))
                (map
                  (lambda (validation-error)
                    (symbol-append ',setting-symbol '- validation-error))
                  validation-errors))))))

      ;; validates a subconfiguration list setting
      (define (validate-subconfiguration-list-setting configuration-symbol setting)
        (let* ((setting-list-symbol (list-ref setting 0))
               (setting-list-validation-parameters (take (drop setting 2) 3))
               (setting-element-type (list-ref setting 5))
               (setting-element-validation-parameters (drop setting 6)))
          `(let ((validation-error
                  (validate-list
                    (,(symbol-append configuration-symbol '- setting-list-symbol) ,configuration-symbol)
                    ,@setting-list-validation-parameters)))
            (if validation-error
              (list (symbol-append ',setting-list-symbol '- validation-error))
              (append-map
                (lambda (element-index)
                  (let ((validation-error
                          (validate-record
                            (list-ref
                              (,(symbol-append configuration-symbol '- setting-list-symbol)
                                ,configuration-symbol)
                              element-index)
                            ,@setting-element-validation-parameters)))
                    (if validation-error
                      (list
                        (symbol-append
                          ',setting-list-symbol '-
                          (string->symbol (number->string element-index)) '-
                          validation-error))
                      (let ((validation-errors
                              (,(symbol-append 'validate- setting-element-type)
                                (list-ref
                                  (,(symbol-append configuration-symbol '- setting-list-symbol)
                                    ,configuration-symbol)
                                  element-index))))
                        (map
                          (lambda (validation-error)
                            (symbol-append
                              ',setting-list-symbol '-
                              (string->symbol (number->string element-index)) '-
                              validation-error))
                          validation-errors)))))
                (iota
                  (length
                    (,(symbol-append configuration-symbol '- setting-list-symbol)
                      ,configuration-symbol))))))))

      ;; parses a value setting from a configuration node
      (define (configuration-node-value-setting configuration-symbol setting)
        (let ((setting-symbol (car setting))
              (setting-default-value
                (if (any (lambda (symbol) (eq? symbol 'default)) (cdr setting))
                  (last setting)
                  #f)))
          `(or
            (configuration-section-get-value
              configuration-node
              ,(symbol->string setting-symbol))
            ,setting-default-value)))

      ;; parses a value list setting from a configuration node
      (define (configuration-node-value-list-setting configuration-symbol setting)
        (let ((setting-symbol (car setting)))
          `(let ((configuration-list-node
                  (configuration-section-get-list
                    configuration-node
                    ,(symbol->string setting-symbol))))
            (if configuration-list-node
              (map
                (lambda (index)
                  (configuration-list-get-value
                    configuration-list-node
                    index))
                (iota (configuration-list-length configuration-list-node)))
              #f))))

      ;; parses a subconfiguration setting from a configuration node
      (define (configuration-node-subconfiguration-setting configuration-symbol setting)
        (let ((setting-symbol (car setting))
              (setting-type (cadr setting)))
          `(let ((configuration-section-node
                  (configuration-section-get-section
                    configuration-node
                    ,(symbol->string setting-symbol))))
            (if configuration-section-node
              (,(symbol-append 'parse- setting-type)
                configuration-section-node)
              #f))))

      ;; parses a subconfiguration list setting from a configuration node
      (define (configuration-node-subconfiguration-list-setting configuration-symbol setting)
        (let* ((setting-symbol (list-ref setting 0))
               (setting-element-type (list-ref setting 5)))
          `(let ((configuration-list-node
                  (configuration-section-get-list
                    configuration-node
                    ,(symbol->string setting-symbol))))
            (if configuration-list-node
              (map
                (lambda (index)
                  (let ((configuration-section-node
                          (configuration-list-get-section
                            configuration-list-node
                            index)))
                    (if configuration-section-node
                      (,(symbol-append 'parse- setting-element-type) configuration-section-node)
                      #f)))
                (iota (configuration-list-length configuration-list-node)))
              #f))))

      ;; parses the expression
      (let* ((configuration-symbol (cadr exp))
             (settings (cddr exp))
             (settings-symbol (map car settings)))
        `(begin

          (import srfi-1)

          (declare (uses configuration))
          (declare (uses validation))

          ;; encapsulates a configuration
          (define-record ,(symbol-append configuration-symbol) ,@settings-symbol)

          ;; settings are untyped before validation
          (: ,(symbol-append 'make- configuration-symbol) (
            ,@(make-list (length settings) `*) -> (struct ,configuration-symbol)))
          ,@(map
            (lambda (setting)
              `(: ,(symbol-append configuration-symbol '- (car setting)) (
                (struct ,configuration-symbol) -> *)))
            settings)
          ,@(map
            (lambda (setting)
              `(: ,(symbol-append configuration-symbol '- (car setting) '-set!) (
                (struct ,configuration-symbol) * -> noreturn)))
            settings)

          ;; once validation has passed
          ;; fields are known to be of the right type
          ;; and should be accessed by these getters
          ,@(concatenate
            (map
              (lambda (setting)
                (list
                  `(: ,(symbol-append configuration-symbol '- (car setting) '*) (
                    (struct ,configuration-symbol) -> ,(setting-type->scheme-type (drop setting 1))))
                  `(define (,(symbol-append configuration-symbol '- (car setting) '*) ,configuration-symbol)
                     (,(symbol-append configuration-symbol '- (car setting)) ,configuration-symbol))))
              settings))

          ;; validates a configuration
          (: ,(symbol-append 'validate- configuration-symbol) (
            (struct ,configuration-symbol) -> (list-of symbol)))
          (define (,(symbol-append 'validate- configuration-symbol) ,configuration-symbol)
            (filter
              (lambda (validation-error) validation-error)
              (append
                ,@(map
                  (lambda (setting)
                    (let ((setting-type (cadr setting)))
                      (cond ((eq? setting-type 'list)
                              (if (string-contains (symbol->string (list-ref setting 5)) "subconfiguration")
                                (validate-subconfiguration-list-setting configuration-symbol setting)
                                (validate-value-list-setting configuration-symbol setting)))
                            ((string-contains (symbol->string setting-type) "subconfiguration")
                              (validate-subconfiguration-setting configuration-symbol setting))
                            (else
                              (validate-value-setting configuration-symbol setting)))))
                  settings))))

          ;; parses a configuration node
          (: ,(symbol-append 'parse- configuration-symbol) (
            (struct configuration-node) -> (struct ,configuration-symbol)))
          (define (,(symbol-append 'parse- configuration-symbol) configuration-node)
            (,(symbol-append 'make- configuration-symbol)
              ,@(map
                (lambda (setting)
                  (let ((setting-type (cadr setting)))
                    (cond ((eq? setting-type 'list)
                            (if (string-contains (symbol->string (list-ref setting 5)) "subconfiguration")
                              (configuration-node-subconfiguration-list-setting configuration-symbol setting)
                              (configuration-node-value-list-setting configuration-symbol setting)))
                          ((string-contains (symbol->string setting-type) "subconfiguration")
                            (configuration-node-subconfiguration-setting configuration-symbol setting))
                          (else
                            (configuration-node-value-setting configuration-symbol setting)))))
                settings))))))))
