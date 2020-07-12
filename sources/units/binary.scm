(import srfi-4)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit binary))

(declare (uses binary-intern))
(declare (uses cbor))
(declare (uses exceptions))

;; encapsulates a binary packer
(define-typed-record binary-packer
  (cbor-item-t* (struct cbor-item-t*)))

;; invokes a procedure with a binary packer
(: with-binary-packer (forall (r) (((struct binary-packer) -> r) -> r)))
(define (with-binary-packer procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((cbor-item-t* (cbor-new-indefinite-array)))
        (unless cbor-item-t*
          (abort "failed to cbor-new-indefinite-array"))
        cbor-item-t*))
    (lambda (cbor-item-t*)
      (procedure
        (make-binary-packer
          cbor-item-t*)))
    cbor-intermediate-decref))

;; adds a boolean value to a binary packer
(: binary-packer-add-boolean ((struct binary-packer) boolean -> noreturn))
(define (binary-packer-add-boolean binary-packer value)
  (binary-packer-add binary-packer #f
    (lambda ()
      (let ((cbor-item-t* (cbor-build-bool value)))
        (unless cbor-item-t*
          (abort "failed to cbor-build-bool"))
        cbor-item-t*))))

;; adds an integer value to a binary packer
(: binary-packer-add-integer ((struct binary-packer) (or fixnum false) -> noreturn))
(define (binary-packer-add-integer binary-packer value)
  (binary-packer-add binary-packer (not value)
    (lambda ()
      (let ((cbor-item-t* (cbor-build-uint32 (abs value))))
        (unless cbor-item-t*
          (abort "failed to cbor-build-uint32"))
        (if (< value 0)
          (cbor-mark-negint cbor-item-t*))
        cbor-item-t*))))

;; adds a double value to a binary packer
(: binary-packer-add-double ((struct binary-packer) (or float false) -> noreturn))
(define (binary-packer-add-double binary-packer value)
  (binary-packer-add binary-packer (not value)
    (lambda ()
      (let ((cbor-item-t* (cbor-build-float8 value)))
        (unless cbor-item-t*
          (abort "failed to cbor-build-float8"))
        cbor-item-t*))))

;; adds a string value to a binary packer
(: binary-packer-add-string ((struct binary-packer) (or string false) -> noreturn))
(define (binary-packer-add-string binary-packer value)
  (binary-packer-add binary-packer (not value)
    (lambda ()
      (let* ((bytes (blob->u8vector (string->blob value)))
             (cbor-item-t* (cbor-build-bytestring bytes (u8vector-length bytes))))
        (unless cbor-item-t*
          (abort "failed to cbor-build-bytestring"))
        cbor-item-t*))))

;; adds bytes to a binary packer
(: binary-packer-add-bytes ((struct binary-packer) (or u8vector false) -> noreturn))
(define (binary-packer-add-bytes binary-packer value)
  (binary-packer-add binary-packer (not value)
    (lambda ()
      (let ((cbor-item-t* (cbor-build-bytestring value (u8vector-length value))))
        (unless cbor-item-t*
          (abort "failed to cbor-build-bytestring"))
        cbor-item-t*))))

;; returns data from a binary packer
(: binary-packer-data ((struct binary-packer) -> u8vector))
(define (binary-packer-data binary-packer)
  (with-guaranteed-release
    (lambda ()
      (let ((cbor-serialize-data* (cbor-serialize-alloc (binary-packer-cbor-item-t* binary-packer))))
        (unless cbor-serialize-data*
          (abort "failed to serialize binary-packer-cbor-item-t*"))
        cbor-serialize-data*))
    (lambda (cbor-serialize-data*)
      (let ((u8vector (make-u8vector (cbor-serialize-data-size cbor-serialize-data*))))
        (cbor-serialize-data-copy cbor-serialize-data* u8vector)
        u8vector))
    cbor-serialize-data-free))

;; encapsulates a binary unpacker
(define-typed-record binary-unpacker
  (cbor-item-t* (struct cbor-item-t*))
  (index fixnum))

;; invokes a procedure with a binary unpacker
(: with-binary-unpacker (forall (r) (u8vector ((struct binary-unpacker) -> r) -> r)))
(define (with-binary-unpacker data procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((cbor-item-t* (cbor-load data (u8vector-length data))))
        (unless cbor-item-t*
          (abort
            (format "failed to load cbor-item-t* with size ~A"
              (u8vector-length data))))
        (unless (eq? (cbor-typeof cbor-item-t*) cbor-type-array)
          (abort "failed to unpack array"))
        (unless (cbor-array-is-indefinite cbor-item-t*)
          (abort "failed to unpack indefinite array"))
        cbor-item-t*))
    (lambda (cbor-item-t*)
      (procedure
        (make-binary-unpacker
          cbor-item-t*
          0)))
    cbor-intermediate-decref))

;; returns an unpacked boolean
(: binary-unpacker-get-boolean ((struct binary-unpacker) -> boolean))
(define (binary-unpacker-get-boolean binary-unpacker)
  (with-binary-unpacker-next binary-unpacker
    (lambda (cbor-item-t*)
      (let ((cbor-type (cbor-typeof cbor-item-t*)))
        (unless (eq? cbor-type cbor-type-decimals-and-ctrl)
          (abort
            (format "failed to unpack boolean got ~A instead"
              cbor-type))))
      (cbor-is-bool cbor-item-t*))))

;; returns an unpacked integer
(: binary-unpacker-get-integer ((struct binary-unpacker) -> (or fixnum false)))
(define (binary-unpacker-get-integer binary-unpacker)
  (with-binary-unpacker-next binary-unpacker
    (lambda (cbor-item-t*)
      (let ((cbor-type (cbor-typeof cbor-item-t*)))
        (cond ((eq? cbor-type cbor-type-positive-integer)
                 (cbor-get-uint32 cbor-item-t*))
              ((eq? cbor-type cbor-type-negative-integer)
                 (* -1 (cbor-get-uint32 cbor-item-t*)))
              (else
                (abort
                  (format "failed to unpack integer got ~A instead"
                    cbor-type))))))))

;; returns an unpacked double
(: binary-unpacker-get-double ((struct binary-unpacker) -> (or number false)))
(define (binary-unpacker-get-double binary-unpacker)
  (with-binary-unpacker-next binary-unpacker
    (lambda (cbor-item-t*)
      (let ((cbor-type (cbor-typeof cbor-item-t*)))
        (unless (eq? cbor-type cbor-type-decimals-and-ctrl)
          (abort
            (format "failed to unpack double got ~A instead"
              cbor-type))))
      (cbor-float-get-float8 cbor-item-t*))))

;; returns an unpacked string
(: binary-unpacker-get-string ((struct binary-unpacker) -> (or string false)))
(define (binary-unpacker-get-string binary-unpacker)
  (with-binary-unpacker-next binary-unpacker
    (lambda (cbor-item-t*)
      (let ((cbor-type (cbor-typeof cbor-item-t*)))
        (unless (eq? cbor-type cbor-type-byte-string)
          (abort
            (format "failed to unpack string got ~A instead"
              cbor-type))))
      (let ((bytes (make-u8vector (cbor-bytestring-length cbor-item-t*) 0)))
        (cbor-bytestring-handle cbor-item-t* bytes)
        (blob->string (u8vector->blob bytes))))))

;; returns unpacked bytes
(: binary-unpacker-get-bytes ((struct binary-unpacker) -> (or u8vector false)))
(define (binary-unpacker-get-bytes binary-unpacker)
  (with-binary-unpacker-next binary-unpacker
    (lambda (cbor-item-t*)
      (let ((cbor-type (cbor-typeof cbor-item-t*)))
        (unless (eq? cbor-type cbor-type-byte-string)
          (abort
            (format "failed to unpack bytes got ~A instead"
              cbor-type))))
      (let ((bytes (make-u8vector (cbor-bytestring-length cbor-item-t*) 0)))
        (cbor-bytestring-handle cbor-item-t* bytes)
        bytes))))
