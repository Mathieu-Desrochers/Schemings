(import srfi-1)
(import srfi-4)
(import srfi-13)

(import (chicken condition))
(import (chicken format))

(declare (unit http-client))

(declare (uses curl))
(declare (uses http-client-intern))

;; encapsulates an http client request
(define-typed-record http-client-request
  (method string)
  (url string)
  (username (or string false))
  (password (or string false))
  (headers (or (list-of string) false))
  (body (or string false)))

;; encapsulates an http client response
(define-typed-record http-client-response
  (status-code fixnum)
  (body string))

;; initializes the http client unit
(: http-client-init (-> noreturn))
(define (http-client-init)
  (curl-global-init curl-global-all))

;; url encodes a string
(: http-client-url-encode (string -> string))
(define (http-client-url-encode string)
  (with-curl*
    (lambda (curl*)
      (curl-easy-escape curl* string 0))))

;; performs an http client request
(: http-client-perform ((struct http-client-request) -> (struct http-client-response)))
(define (http-client-perform http-client-request)
  (with-curl*
    (lambda (curl*)

      ;; set the method options
      (cond
        ((equal? (http-client-request-method http-client-request) "PUT")
            (unless
              (eq? curle-ok
                (curl-easy-setopt-string curl* curlopt-customrequest "PUT"))
              (abort "failed to set CURLOPT_CUSTOMREQUEST to PUT")))
        ((equal? (http-client-request-method http-client-request) "DELETE")
            (unless
              (eq? curle-ok
                (curl-easy-setopt-string curl* curlopt-customrequest "DELETE"))
              (abort "failed to set CURLOPT_CUSTOMREQUEST to DELETE"))))

      ;; set the url
      (unless
        (eq? curle-ok
          (curl-easy-setopt-string curl* curlopt-url
            (http-client-request-url http-client-request)))
        (abort
          (format "failed to set CURLOPT_URL to ~A"
            (http-client-request-url http-client-request))))

      ;; set the username and password
      (if (and (http-client-request-username http-client-request)
               (http-client-request-password http-client-request))
        (unless
          (eq? curle-ok
            (curl-easy-setopt-string curl* curlopt-userpwd
              (string-append
                (http-client-request-username http-client-request) ":"
                (http-client-request-password http-client-request))))
          (abort "failed to set CURLOPT_USERPWD to *****:*****")))

      (with-curl-slist**
        (lambda (headers-curl-slist**)

          ;; set the headers
          (if (http-client-request-headers http-client-request)
            (begin
              (for-each
                (lambda (header)
                  (curl-slist-append headers-curl-slist** header))
                (http-client-request-headers http-client-request))
              (unless
                (eq? curle-ok
                  (curl-easy-setopt-strings curl* curlopt-httpheader headers-curl-slist**))
                (abort
                  (format "failed to set CURLOPT_HTTPHEADER to ~A"
                    (http-client-request-headers http-client-request))))))

          ;; set the body
          (if (http-client-request-body http-client-request)
            (unless
              (eq? curle-ok
                (curl-easy-setopt-string curl* curlopt-copypostfields
                  (http-client-request-body http-client-request)))
              (abort
                (format
                  "failed to set CURLOPT_COPYPOSTFIELDS to ~A..."
                  (substring/shared
                    (http-client-request-body http-client-request)
                    0 256)))))

          ;; perform the request
          (let* ((error-code-vector (make-s64vector 1 0))
                 (response-body (curl-easy-perform curl* error-code-vector))
                 (error-code (s64vector-ref error-code-vector 0)))
            (unless (eq? error-code 0)
              (abort
                (format "failed to perform http request ~A ~A with error code ~A"
                  (http-client-request-method http-client-request)
                  (http-client-request-url http-client-request)
                  error-code)))

            ;; get the response code
            (let ((response-code-vector (make-s64vector 1 0)))
              (unless
                (eq? curle-ok
                  (curl-easy-getinfo-long curl* curlinfo-response-code response-code-vector))
                (abort "failed to get CURLINFO_RESPONSE_CODE"))

              ;; make the http client response
              (let ((response-code (s64vector-ref response-code-vector 0)))
                (make-http-client-response
                  response-code
                  response-body)))))))))

;; cleans up the http client unit
(: http-client-cleanup (-> noreturn))
(define (http-client-cleanup)
  (curl-global-cleanup))
