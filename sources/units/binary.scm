(import srfi-4)

(import (chicken blob))
(import (chicken condition))
(import (chicken format))

(declare (unit binary))

(declare (uses cbor))
(declare (uses exceptions))

;; encapsulates a binary packer
(define-typed-record binary-packer
  (cbor-item-t* (struct cbor-item-t*)))

;; invokes a procedure with a binary packer
;; to which count values will be added
(: with-binary-packer (forall (r) (fixnum ((struct binary-packer) -> r) -> r)))
(define (with-binary-packer count procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((cbor-item-t* (cbor-new-definite-array count)))
        (unless cbor-item-t*
          (abort "failed to cbor-new-definite-array"))
        cbor-item-t*))
    (lambda (cbor-item-t*)
      (procedure
        (make-binary-packer
          cbor-item-t*)))
    cbor-intermediate-decref))

;; adds a boolean value to a binary packer
(: binary-packer-add-boolean ((struct binary-packer) boolean -> noreturn))
(define (binary-packer-add-boolean binary-packer value)
  (let ((cbor-item-t* (cbor-build-bool value)))
    (unless cbor-item-t*
      (abort "failed to cbor-build-bool"))
    (unless (cbor-array-push (binary-packer-cbor-item-t* binary-packer) cbor-item-t*)
      (abort "failed to cbor-array-push"))))

;; adds an integer value to a binary packer
(: binary-packer-add-integer ((struct binary-packer) fixnum -> noreturn))
(define (binary-packer-add-integer binary-packer value)
  (let ((cbor-item-t* (cbor-build-uint64 (abs value))))
    (if (< value 0)
      (cbor-mark-negint cbor-item-t*))
    (unless cbor-item-t*
      (abort "failed to cbor-build-uint64"))
    (unless (cbor-array-push (binary-packer-cbor-item-t* binary-packer) cbor-item-t*)
      (abort "failed to cbor-array-push"))) )

;; adds a double value to a binary packer
(: binary-packer-add-double ((struct binary-packer) float -> noreturn))
(define (binary-packer-add-double binary-packer value)
  (let ((cbor-item-t* (cbor-build-float8 value)))
    (unless cbor-item-t*
      (abort "failed to cbor-build-float8"))
    (unless (cbor-array-push (binary-packer-cbor-item-t* binary-packer) cbor-item-t*)
      (abort "failed to cbor-array-push"))))

;; adds a string value to a binary packer
(: binary-packer-add-string ((struct binary-packer) string -> noreturn))
(define (binary-packer-add-string binary-packer value)
  (let ((cbor-item-t* (cbor-build-bytestring value (string-length value))))
    (unless cbor-item-t*
      (abort "failed to cbor-build-bytestring"))
    (unless (cbor-array-push (binary-packer-cbor-item-t* binary-packer) cbor-item-t*)
      (abort "failed to cbor-array-push"))))

;; returns data from a binary packer
(: binary-packer-data ((struct binary-packer) -> u8vector))
(define (binary-packer-data binary-packer)
  (with-guaranteed-release
    (lambda ()
      (cbor-serialize-alloc (binary-packer-cbor-item-t* binary-packer)))
    (lambda (cbor-serialize-data*)
      (let ((u8vector (make-u8vector (cbor-serialize-data-size cbor-serialize-data*))))
        (cbor-serialize-data-copy cbor-serialize-data* u8vector)
        u8vector))
    cbor-serialize-data-free))

;;;; encapsulates a binary unpacker
;;(define-typed-record binary-unpacker
;;  (msgpack-unpacker* (struct msgpack-unpacker*))
;;  (msgpack-unpacked* (struct msgpack-unpacked*)))
;;
;;;; invokes a procedure with a binary unpacker
;;(: with-binary-unpacker (forall (r) (u8vector ((struct binary-unpacker) -> r) -> r)))
;;(define (with-binary-unpacker data procedure)
;;  (with-guaranteed-release
;;    (lambda ()
;;      (let ((msgpack-unpacker* (msgpack-unpacker-new (u8vector-length data))))
;;        (unless msgpack-unpacker*
;;          (abort
;;            (format "failed to create msgpack-unpacker with size ~A"
;;              (u8vector-length data))))
;;        msgpack-unpacker*))
;;    (lambda (msgpack-unpacker*)
;;      (with-guaranteed-release
;;        (lambda ()
;;          (msgpack-unpacker-set-data msgpack-unpacker* data (u8vector-length data))
;;          (let ((msgpack-unpacked* (msgpack-unpacked-new)))
;;            (unless msgpack-unpacked*
;;              (abort "failed to create msgpack-unpacked"))
;;            msgpack-unpacked*))
;;        (lambda (msgpack-unpacked*)
;;          (msgpack-unpacked-init msgpack-unpacked*)
;;          (procedure
;;            (make-binary-unpacker
;;              msgpack-unpacker*
;;              msgpack-unpacked*)))
;;        msgpack-unpacked-free))
;;    msgpack-unpacker-free))
;;
;;;; returns an unpacked boolean
;;(: binary-unpacker-boolean ((struct binary-unpacker) -> boolean))
;;(define (binary-unpacker-boolean binary-unpacker)
;;  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
;;         (msgpack-object-type (msgpack-object-type msgpack-object*)))
;;    (display msgpack-object-type)
;;    (unless (eq? msgpack-object-type 1)
;;      (abort
;;        (format "failed to unpack boolean got ~A instead"
;;          msgpack-object-type)))
;;    (eq? 1
;;      (msgpack-object-boolean
;;        msgpack-object*))))
;;
;;;; returns an unpacked integer
;;(: binary-unpacker-integer ((struct binary-unpacker) -> fixnum))
;;(define (binary-unpacker-integer binary-unpacker)
;;  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
;;         (msgpack-object-type (msgpack-object-type msgpack-object*)))
;;    (display msgpack-object-type)
;;    (unless (or (eq? msgpack-object-type 2) (eq? msgpack-object-type 3))
;;      (abort
;;        (format "failed to unpack integer got ~A instead"
;;          msgpack-object-type)))
;;    (msgpack-object-int
;;      msgpack-object*)))
;;
;;;; returns an unpacked double
;;(: binary-unpacker-double ((struct binary-unpacker) -> number))
;;(define (binary-unpacker-double binary-unpacker)
;;  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
;;         (msgpack-object-type (msgpack-object-type msgpack-object*)))
;;    (display msgpack-object-type)
;;    (unless (or (eq? msgpack-object-type 2)
;;                (eq? msgpack-object-type 3)
;;                (eq? msgpack-object-type 4)
;;                (eq? msgpack-object-type 10))
;;      (abort
;;        (format "failed to unpack double got ~A instead"
;;          msgpack-object-type)))
;;    (msgpack-object-double
;;      msgpack-object*)))
;;
;;;; returns an unpacked string
;;(: binary-unpacker-string ((struct binary-unpacker) -> string))
;;(define (binary-unpacker-string binary-unpacker)
;;  (let* ((msgpack-object* (binary-unpacker-next binary-unpacker))
;;         (msgpack-object-type (msgpack-object-type msgpack-object*)))
;;    (display msgpack-object-type)
;;    (unless (eq? msgpack-object-type 5)
;;      (abort
;;        (format "failed to unpack string got ~A instead"
;;          msgpack-object-type)))
;;    (msgpack-object-string
;;      msgpack-object*)))
