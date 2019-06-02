(import srfi-1)

(import (chicken condition))
(import (chicken format))

(declare (unit http-server))

(declare (uses curl))
(declare (uses fastcgi))
(declare (uses http-server-intern))
(declare (uses sql))

;; encapsulates an http binding
(define-typed-record http-binding
  (method string)
  (route-regex string)
  (request-content-type (or string false))
  (response-content-type (or string false))
  (service-procedure (* (or (struct sql-connection) false) * * -> *))
  (parse-request-procedure ((list-of string) string -> *))
  (format-response-procedure (* -> (or string blob false)))
  (requires-authentication boolean))

;; starts serving http requests
(: http-server-start (
  (list-of (struct http-binding)) (or (struct sql-connection) false)
  (or ((struct fastcgi-request) -> *) false) * ->
  noreturn))
(define (http-server-start
          http-bindings
          sql-connection
          get-authentication-procedure
          configuration)

  ;; compile the http binding regexes
  (with-compiled-http-binding-regexes http-bindings
    (lambda (http-binding-regexes)

      ;; accept the next fastcgi request
      (fastcgi-accept-requests
        (lambda (fastcgi-request)

          ;; match the request against a http binding
          (let ((http-binding-match
                  (find-http-binding-match
                    fastcgi-request
                    http-bindings
                    http-binding-regexes)))

            ;; parse the http binding
            (if http-binding-match
              (let* ((http-binding (car http-binding-match))
                     (request-content-type (http-binding-request-content-type http-binding))
                     (response-content-type (http-binding-response-content-type http-binding))
                     (service-procedure (http-binding-service-procedure http-binding))
                     (parse-request-procedure (http-binding-parse-request-procedure http-binding))
                     (format-response-procedure (http-binding-format-response-procedure http-binding))
                     (requires-authentication (http-binding-requires-authentication http-binding))
                     (route-captures (cddr http-binding-match)))

                ;; check the authentication
                ;; if required by the http binding
                (let ((authentication
                        (and requires-authentication (get-authentication-procedure fastcgi-request))))
                  (if (and requires-authentication (not authentication))
                    (begin
                      (fastcgi-write-response-line fastcgi-request "Status: 401 Unauthorized")
                      (fastcgi-write-response-line fastcgi-request ""))

                    ;; read and parse the request
                    (let* ((fastcgi-request-body
                              (cond
                                ((not request-content-type) #f)
                                ((equal? request-content-type "application/json; charset=utf-8")
                                  (blob->string (fastcgi-read-request-body fastcgi-request)))
                                ((equal? request-content-type "application/octet-stream")
                                  (fastcgi-read-request-body fastcgi-request))
                                (else
                                  (abort
                                    (format "unsupported request-content-type ~A"
                                      request-content-type)))))
                           (request
                             (parse-request-procedure route-captures fastcgi-request-body)))
                      (if request

                        ;; invoke the service within a database transaction
                        (http-with-validation-errors-exception-handling
                          (lambda ()
                            (let ((response
                                    (if sql-connection
                                      (within-sql-transaction sql-connection
                                        (lambda ()
                                          (service-procedure
                                            request sql-connection authentication configuration)))
                                      (service-procedure
                                        request sql-connection authentication configuration))))

                              ;; format and write the response
                              (if response-content-type
                                (let ((fastcgi-response-body (format-response-procedure response)))
                                  (begin
                                    (fastcgi-write-response-line fastcgi-request "Status: 200 OK")
                                    (fastcgi-write-response-line fastcgi-request
                                      (string-append "Content-Type: " response-content-type))
                                    (fastcgi-write-response-line fastcgi-request "")
                                    (cond
                                      ((equal? response-content-type "application/json; charset=utf-8")
                                        (fastcgi-write-response-line fastcgi-request fastcgi-response-body))
                                      ((or (equal? response-content-type "application/octet-stream")
                                           (equal? response-content-type "application/pdf"))
                                        (fastcgi-write-response-blob fastcgi-request fastcgi-response-body))
                                      (else
                                        (abort
                                          (format "unsupported response-content-type ~A"
                                            response-content-type))))))
                                (begin
                                  (fastcgi-write-response-line fastcgi-request "Status: 204 No Content")
                                  (fastcgi-write-response-line fastcgi-request "")))))

                          ;; the request validation failed
                          (lambda (validation-errors)
                            (fastcgi-write-response-line fastcgi-request "Status: 422 Unprocessable Entity")
                            (fastcgi-write-response-line fastcgi-request
                              (string-append "Content-Type: application/json; charset=utf-8"))
                            (fastcgi-write-response-line fastcgi-request "")
                            (fastcgi-write-response-line fastcgi-request
                              (http-format-validation-errors validation-errors))))

                        ;; the request could not be parsed
                        (begin
                          (fastcgi-write-response-line fastcgi-request "Status: 400 Bad Request")
                          (fastcgi-write-response-line fastcgi-request "")))))))

                ;; an http binding could not be matched
                (begin
                  (fastcgi-write-response-line fastcgi-request "Status: 404 Not Found")
                  (fastcgi-write-response-line fastcgi-request "")))))))))
