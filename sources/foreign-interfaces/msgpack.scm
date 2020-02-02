(declare (unit msgpack))

(foreign-declare "

#include <msgpack.h>

msgpack_packer* msgpack_packer_new_wrapped(msgpack_sbuffer* buffer)
{
  return msgpack_packer_new(buffer, msgpack_sbuffer_write);
}

")

;; msgpack-sbuffer pointers definitions
(define-foreign-type msgpack-sbuffer "msgpack_sbuffer")
(define-foreign-type msgpack-sbuffer* (c-pointer msgpack-sbuffer))

;; msgpack-packer pointers definitions
(define-foreign-type msgpack-packer "msgpack_packer")
(define-foreign-type msgpack-packer* (c-pointer msgpack-packer))

;; create and initialize a sbuffer
(define msgpack-sbuffer-new (foreign-lambda msgpack-sbuffer* "msgpack_sbuffer_new"))
(define msgpack-sbuffer-init (foreign-lambda void "msgpack_sbuffer_init" msgpack-sbuffer*))
(define msgpack-sbuffer-free (foreign-lambda void "msgpack_sbuffer_free" msgpack-sbuffer*))

;; create a packer
(define msgpack-packer-new (foreign-lambda msgpack-packer* "msgpack_packer_new_wrapped" msgpack-sbuffer*))
(define msgpack-packer-free (foreign-lambda void "msgpack_packer_free" msgpack-packer*))
