(declare (unit msgpack))

(foreign-declare "

#include <msgpack.h>

// creates a packer
msgpack_packer* msgpack_packer_new_wrapped(msgpack_sbuffer* buffer)
{
  return msgpack_packer_new(buffer, msgpack_sbuffer_write);
}

// returns a sbuffer size
int msgpack_sbuffer_size(msgpack_sbuffer* buffer)
{
  return buffer->size;
}

// returns a sbuffer data
char* msgpack_sbuffer_data(msgpack_sbuffer* buffer)
{
  return buffer->data;
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

;; buffer getters
(define msgpack-sbuffer-size (foreign-lambda int "msgpack_sbuffer_size" msgpack-sbuffer*))
(define msgpack-sbuffer-data (foreign-lambda c-string "msgpack_sbuffer_data" msgpack-sbuffer*))

;; create a packer
(define msgpack-packer-new (foreign-lambda msgpack-packer* "msgpack_packer_new_wrapped" msgpack-sbuffer*))
(define msgpack-packer-free (foreign-lambda void "msgpack_packer_free" msgpack-packer*))

;; packing
(define msgpack-pack-int (foreign-lambda int "msgpack_pack_int" msgpack-packer* int))
(define msgpack-pack-double (foreign-lambda int "msgpack_pack_double" msgpack-packer* double))
(define msgpack-pack-nil (foreign-lambda int "msgpack_pack_nil" msgpack-packer*))
(define msgpack-pack-true (foreign-lambda int "msgpack_pack_true" msgpack-packer*))
(define msgpack-pack-false (foreign-lambda int "msgpack_pack_false" msgpack-packer*))
(define msgpack-pack-array (foreign-lambda int "msgpack_pack_array" msgpack-packer* int))
(define msgpack-pack-str (foreign-lambda int "msgpack_pack_str" msgpack-packer* int))
(define msgpack-pack-str-body (foreign-lambda int "msgpack_pack_str_body" msgpack-packer* c-string int))
