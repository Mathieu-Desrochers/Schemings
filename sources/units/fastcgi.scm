(import srfi-4)
(import srfi-13)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit fastcgi))

(declare (uses fastcgi-intern))
(declare (uses fcgi))
(declare (uses vector))

;; starts accepting fastcgi requests
(: fastcgi-accept-requests (((struct fastcgi-request) -> *) -> noreturn))
(define (fastcgi-accept-requests procedure)
  (with-fastcgi-request-pointers
    (lambda (in-fcgx-stream** out-fcgx-stream** err-fcgx-stream** fcgx-paramarray*)
      (letrec* ((fastcgi-accept-next-request
                  (lambda ()
                    (let ((fcgx-accept-result
                            (fcgx-accept
                              in-fcgx-stream**
                              out-fcgx-stream**
                              err-fcgx-stream**
                              fcgx-paramarray*)))
                      (if (eq? fcgx-accept-result 0)
                        (begin
                          (procedure
                            (make-fastcgi-request
                              (resolve-fcgx-stream* in-fcgx-stream**)
                              (resolve-fcgx-stream* out-fcgx-stream**)
                              (resolve-fcgx-stream* err-fcgx-stream**)
                              (resolve-fcgx-paramarray fcgx-paramarray*)))
                          (fcgx-finish)
                          (fastcgi-accept-next-request))
                        (unless (eq? fcgx-accept-result fcgx-accept-eintr)
                          (abort
                            (format
                              "failed to accept fastcgi request with error ~A"
                              fcgx-accept-result))))))))
        (fastcgi-accept-next-request)))))

;; returns the authorization header of a fastcgi request
(: fastcgi-request-authorization-header ((struct fastcgi-request) -> string))
(define (fastcgi-request-authorization-header fastcgi-request)
  (let ((fcgx-paramarray (fastcgi-request-fcgx-paramarray fastcgi-request)))
    (fcgx-getparam "HTTP_AUTHORIZATION" fcgx-paramarray)))

;; returns the method of a fastcgi request
(: fastcgi-request-method ((struct fastcgi-request) -> string))
(define (fastcgi-request-method fastcgi-request)
  (let ((fcgx-paramarray (fastcgi-request-fcgx-paramarray fastcgi-request)))
    (fcgx-getparam "REQUEST_METHOD" fcgx-paramarray)))

;; returns the uri of a fastcgi request
(: fastcgi-request-uri ((struct fastcgi-request) -> string))
(define (fastcgi-request-uri fastcgi-request)
  (let* ((fcgx-paramarray (fastcgi-request-fcgx-paramarray fastcgi-request))
         (request-uri (fcgx-getparam "REQUEST_URI" fcgx-paramarray))
         (script-name (fcgx-getparam "SCRIPT_NAME" fcgx-paramarray)))
    (string-drop request-uri (string-length script-name))))

;; reads the body of a fastcgi request
(: fastcgi-read-request-body ((struct fastcgi-request) -> blob))
(define (fastcgi-read-request-body fastcgi-request)
  (let ((in-fcgx-stream* (fastcgi-request-in-fcgx-stream* fastcgi-request)))
    (letrec ((read-chunks
                (lambda (accumulated-vectors accumulated-length chunk-size)
                  (let* ((chunk-vector (make-u8vector chunk-size))
                         (chunk-length (fcgx-getstr chunk-vector chunk-size in-fcgx-stream*)))
                    (if (< chunk-length chunk-size)
                      (cons
                        (cons chunk-vector accumulated-vectors)
                        (+ chunk-length accumulated-length))
                      (read-chunks
                        (cons chunk-vector accumulated-vectors)
                        (+ chunk-length accumulated-length)
                        (* chunk-size 2)))))))
      (let* ((chunks (read-chunks (list) 0 1024))
             (chunks-vectors (reverse (car chunks)))
             (chunks-length (cdr chunks)))
        (u8vector->blob
          (vector-concatenate chunks-vectors chunks-length))))))

;; writes a string followed by a line feed to a fastcgi request
(: fastcgi-write-response-line ((struct fastcgi-request) string -> noreturn))
(define (fastcgi-write-response-line fastcgi-request string)
  (let ((out-fcgx-stream* (fastcgi-request-out-fcgx-stream* fastcgi-request)))
    (fcgx-puts string out-fcgx-stream*)
    (fcgx-puts "\r\n" out-fcgx-stream*)))

;; writes a blob to a fastcgi request
(: fastcgi-write-response-blob ((struct fastcgi-request) blob -> noreturn))
(define (fastcgi-write-response-blob fastcgi-request blob)
  (let ((out-fcgx-stream* (fastcgi-request-out-fcgx-stream* fastcgi-request)))
    (fcgx-putstr blob (blob-size blob) out-fcgx-stream*)))
