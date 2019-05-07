(declare (unit sodium))

(foreign-declare "

#include <sodium.h>

")

;; constants
(define sodium-generic-hash-bytes (foreign-value "crypto_generichash_BYTES" int))

;; initializes the library
(define sodium-init (foreign-lambda int "sodium_init"))

;; returns an unpredictable value between 0 and upper_bound
(define sodium-randombytes-uniform (foreign-lambda int "randombytes_uniform" int))

;; returns the fingerprint of a message
(define sodium-generic-hash
  (foreign-lambda int "crypto_generichash"
    u8vector int u8vector long (const unsigned-c-string) long))
