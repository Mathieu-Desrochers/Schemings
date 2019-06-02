(import srfi-1)

(declare (unit http-server-intern))

(declare (uses exceptions))
(declare (uses fastcgi))
(declare (uses json))
(declare (uses regex))
(declare (uses validation))

;; invokes a procedure with compiled http binding regexes
(: with-compiled-http-binding-regexes
  (forall (r) (
    (list-of (struct http-binding)) ((list-of (struct regex)) -> r) -> r)))
(define (with-compiled-http-binding-regexes http-bindings procedure)
  (with-guaranteed-release
    (lambda ()
      (map
        (lambda (http-binding)
          (regex-compile (http-binding-route-regex http-binding)))
        http-bindings))
    (lambda (http-binding-regexes)
      (procedure http-binding-regexes))
    (lambda (http-binding-regexes)
      (for-each
        (lambda (http-binding-regex)
          (regex-free http-binding-regex))
        http-binding-regexes))))

;; searches for a http binding matching a fastcgi request
(: find-http-binding-match (
  (struct fastcgi-request) (list-of (struct http-binding)) (list-of (struct regex)) ->
  (or (pair (struct http-binding) (list-of string)) false)))
(define (find-http-binding-match fastcgi-request http-bindings http-binding-regexes)
  (let ((fastcgi-request-method (fastcgi-request-method fastcgi-request))
        (fastcgi-request-uri (fastcgi-request-uri fastcgi-request)))
    (letrec* ((find-http-binding-match-iter
                (lambda (zipped)
                  (if (not (null? zipped))
                    (let* ((http-binding (caar zipped))
                           (http-binding-method (http-binding-method http-binding))
                           (http-binding-regex (cadar zipped)))
                      (if (equal? http-binding-method fastcgi-request-method)
                        (let ((regex-captures
                                (regex-execute-compiled
                                  http-binding-regex
                                  fastcgi-request-uri)))
                          (if (not (null? regex-captures))
                            (cons http-binding regex-captures)
                            (find-http-binding-match-iter (cdr zipped))))
                        (find-http-binding-match-iter (cdr zipped))))
                    #f))))
      (find-http-binding-match-iter
        (zip http-bindings http-binding-regexes)))))

;; invokes a procedure with validation errors exception handling
(: http-with-validation-errors-exception-handling
  (forall (r) ((-> r) ((list-of symbol) -> *) -> r)))
(define (http-with-validation-errors-exception-handling procedure exception-procedure)
  (handle-exceptions
    exception
    (if (validation-errors-exception? exception)
      (let ((validation-errors (validation-errors-exception-validation-errors exception)))
        (exception-procedure validation-errors))
      (abort exception))
    (procedure)))

;; formats a list of validation errors
(: http-format-validation-errors ((list-of symbol) -> string))
(define (http-format-validation-errors validation-errors)
  (with-json-array
    (lambda (json-node)
      (for-each
        (lambda (validation-error)
          (json-array-add-value json-node (symbol->string validation-error)))
        validation-errors)
      (json->string json-node))))
