(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit binary-intern))

(declare (uses cbor))
(declare (uses exceptions))

;; adds an item to a binary packer
(: binary-packer-add (forall (r) (boolean (struct binary-packer) (-> r) -> r)))
(define (binary-packer-add binary-packer null-value? procedure)
  (with-guaranteed-release
    (lambda ()
      (if null-value?
        (let ((cbor-item-t* (cbor-new-null)))
          (unless cbor-item-t*
            (abort "failed to cbor-new-null"))
          cbor-item-t*)
        (procedure)))
    (lambda (cbor-item-t*)
      (unless (cbor-array-push (binary-packer-cbor-item-t* binary-packer) cbor-item-t*)
        (abort "failed to cbor-array-push")))
    cbor-intermediate-decref))

;; invokes a procedure with the next unpacked item
(: with-binary-unpacker-next (forall (r) ((struct binary-unpacker) ((struct cbor-item-t*) -> r) -> r)))
(define (with-binary-unpacker-next binary-unpacker procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((cbor-item-t* (binary-unpacker-cbor-item-t* binary-unpacker))
            (index (binary-unpacker-index binary-unpacker)))
        (if (>= index (cbor-array-size cbor-item-t*))
          (abort
            (format "failed to unpack index ~A is out of bounds"
              index)))
        (let ((cbor-item-t* (cbor-array-get cbor-item-t* index)))
          (unless cbor-item-t*
            (abort (format "failed to unpack index ~A"
              index)))
          (binary-unpacker-index-set!
            binary-unpacker
            (+ index 1))
          cbor-item-t*)))
    (lambda (cbor-item-t*)
      (if (not (cbor-is-null cbor-item-t*))
        (procedure cbor-item-t*)
        #f))
    cbor-intermediate-decref))
