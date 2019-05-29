(import srfi-13)

(declare (unit signed-token))

(declare (uses crypto))
(declare (uses date-time))
(declare (uses regex))

;; signs a token
(: sign-token (string string -> string))
(define (sign-token token secret)
  (let ((timestamped-token (string-append token "." (date-time->string (date-time-now)))))
    (string-append
      timestamped-token
      "."
      (crypto-hash-string
        timestamped-token
        secret))))

;; validates a signed token
(: signed-token-validate (string string fixnum -> boolean))
(define (signed-token-validate signed-token secret validity-in-seconds)
  (if (not signed-token)
    #f
    (let* ((pattern "^(.*)\\.(\\d{4}\\-\\d{2}\\-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z)\\.(.*)$")
           (regex-matches (regex-execute pattern signed-token)))
      (if (null? regex-matches)
        #f
        (let ((expected-signature
                (crypto-hash-string
                  (string-append (list-ref regex-matches 1) "." (list-ref regex-matches 2))
                  secret)))
          (if (not (equal? (list-ref regex-matches 3) expected-signature))
            #f
            (let ((timestamp (try-string->date-time (list-ref regex-matches 2))))
              (if (not timestamp)
                #f
                (if (< 0 (date-time-diff
                            (date-time-add timestamp validity-in-seconds)
                            (date-time-now)))
                  #f
                  (list-ref regex-matches 1))))))))))
