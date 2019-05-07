(import srfi-4)
(import srfi-13)

(import (chicken blob))

(declare (unit crypto))

(declare (uses base64))
(declare (uses sodium))

;; initializes the crypto module
(: crypto-init (-> noreturn))
(define (crypto-init)
  (let ((sodium-init-result (sodium-init)))
    (unless (eq? sodium-init-result 0)
      (abort
        (format "failed to initialize crypto module with error ~A"
          sodium-init-result)))))

;; returns a random number
(: crypto-random-number (fixnum -> fixnum))
(define (crypto-random-number upper-bound)
  (sodium-randombytes-uniform upper-bound))

;; returns a random string
(: crypto-random-string (fixnum -> string))
(define (crypto-random-string length)
  (let ((u8vector (make-u8vector length)))
    (letrec (
        (fill-vector
          (lambda (index)
            (if (< index length)
              (begin
                (u8vector-set! u8vector index (sodium-randombytes-uniform 256))
                (fill-vector (+ index 1)))))))
      (fill-vector 0)
      (substring/shared (base64-encode (u8vector->blob u8vector)) 0 length))))

;; computes the hash of bytes
(: crypto-hash-bytes (blob string -> string))
(define (crypto-hash-bytes blob secret)
  (let* ((blob-u8vector (blob->u8vector blob))
         (hash-u8vector (make-u8vector sodium-generic-hash-bytes))
         (hash-result
            (sodium-generic-hash
              hash-u8vector
              sodium-generic-hash-bytes
              blob-u8vector
              (u8vector-length blob-u8vector)
              secret
              (string-length secret))))
    (unless (eq? hash-result 0)
      (abort
        (format "failed to hash bytes with error ~A"
          hash-result)))
    (base64-encode (u8vector->blob hash-u8vector))))

;; computes the hash of a string
(: crypto-hash-string (string string -> string))
(define (crypto-hash-string string secret)
  (crypto-hash-bytes (string->blob string) secret))
