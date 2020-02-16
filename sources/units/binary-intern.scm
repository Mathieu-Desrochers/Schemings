(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit binary-intern))

(declare (uses cbor))
(declare (uses exceptions))

;; adds an item to a binary packer
(: binary-packer-add (forall (r) ((struct binary-packer) boolean (-> r) -> r)))
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
      (if (not (and
                 (cbor-isa-float-ctrl cbor-item-t*)
                 (cbor-float-ctrl-is-ctrl cbor-item-t*)
                 (cbor-is-null cbor-item-t*)))
        (procedure cbor-item-t*)
        #f))
    cbor-intermediate-decref))

;; adds a field value to a binary packer
(: binary-packer-add-field ((struct binary-packer) * symbol -> noreturn))
(define (binary-packer-add-field binary-packer value field-type)
  (cond ((eq? field-type 'blob)
          (binary-packer-add-bytes binary-packer value))
        ((eq? field-type 'boolean)
          (binary-packer-add-boolean binary-packer value))
        ((eq? field-type 'integer)
          (binary-packer-add-integer binary-packer value))
        ((eq? field-type 'number)
          (binary-packer-add-double binary-packer value))
        ((eq? field-type 'string)
          (binary-packer-add-string binary-packer value))
        ((eq? field-type 'date)
          (binary-packer-add-string binary-packer (date->string value)))
        ((eq? field-type 'date-time)
          (binary-packer-add-string binary-packer (date-time->string value)))
        ((eq? field-type 'time)
          (binary-packer-add-string binary-packer (time->string value)))
        (else
          (abort
            (format "failed to binary pack field of type ~A"
              field-type)))))

;; returns an unpacked field
(: binary-packer-get-field ((struct binary-unpacker) symbol -> *))
(define (binary-unpacker-get-field binary-unpacker field-type)
  (cond ((eq? field-type 'blob)
          (binary-unpacker-get-bytes binary-unpacker))
        ((eq? field-type 'boolean)
          (binary-unpacker-get-boolean binary-unpacker))
        ((eq? field-type 'integer)
          (binary-unpacker-get-integer binary-unpacker))
        ((eq? field-type 'number)
          (binary-unpacker-get-double binary-unpacker))
        ((eq? field-type 'string)
          (binary-unpacker-get-string binary-unpacker))
        ((eq? field-type 'date)
          (string->date (binary-unpacker-get-string binary-unpacker)))
        ((eq? field-type 'date-time)
          (string->date-time (binary-unpacker-get-string binary-unpacker)))
        ((eq? field-type 'time)
          (string->time (binary-unpacker-get-string binary-unpacker)))
        (else
          (abort
            (format "failed to binary unpack field of type ~A"
              field-type)))))
