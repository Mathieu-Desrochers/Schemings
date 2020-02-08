(import srfi-4)

(import (chicken condition))

(declare (unit binary-inner))

(declare (uses exceptions))
(declare (uses msgpack))

;; unpacks the next value
(: binary-unpacker-next ((struct binary-packer) -> pointer))
(define (binary-unpacker-next binary-unpacker)
  (unless (eq? 2
            (msgpack-unpack-next
              (binary-unpacker-msgpack-unpacked* binary-unpacker)
              (binary-unpacker-data binary-unpacker)
              (u8vector-length (binary-unpacker-data binary-unpacker))
              (binary-unpacker-offset binary-unpacker)))
    (abort "failed to step msgpack-unpacked"))
  (msgpack-unpacked-object
    (binary-unpacker-msgpack-unpacked*
      binary-unpacker)))
