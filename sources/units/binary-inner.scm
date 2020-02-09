(import srfi-4)

(import (chicken condition))
(import (chicken format))

(declare (unit binary-inner))

(declare (uses exceptions))
(declare (uses msgpack))

;; unpacks the next value
(: binary-unpacker-next ((struct binary-packer) -> pointer))
(define (binary-unpacker-next binary-unpacker)
  (let ((msgpack-unpacker-next-result
          (msgpack-unpacker-next
            (binary-unpacker-msgpack-unpacker* binary-unpacker)
            (binary-unpacker-msgpack-unpacked* binary-unpacker))))
    (unless (>= msgpack-unpacker-next-result 0)
      (abort
        (format "failed to step msgpack-unpacker with error ~A"
          msgpack-unpacker-next-result)))
    (msgpack-unpacked-object
      (binary-unpacker-msgpack-unpacked*
        binary-unpacker))))
