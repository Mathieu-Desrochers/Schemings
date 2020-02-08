(declare (unit msgpack))

(foreign-declare "

#include <msgpack.h>

// creates a packer
msgpack_packer* msgpack_packer_new_wrapped(msgpack_sbuffer* buffer)
{
  return msgpack_packer_new(buffer, msgpack_sbuffer_write);
}

// returns a sbuffer data
char* msgpack_sbuffer_data(msgpack_sbuffer* buffer)
{
  char* data = calloc(buffer->size + 1, sizeof(char));
  memcpy(data, buffer->data, buffer->size);
  return data;
}

// creates an unpacked
msgpack_unpacked* msgpack_unpacked_new()
{
  return malloc(sizeof(msgpack_unpacked));
}

// returns an unpacked current object
msgpack_object* msgpack_unpacked_object(msgpack_unpacked* unpacked)
{
  return &(unpacked->data);
}

// returns an object type
int msgpack_object_type_getter(msgpack_object* object)
{
  return object->type;
}

// returns an object value
int msgpack_object_int(msgpack_object* object) { return (int)object->via.i64; }
double msgpack_object_double(msgpack_object* object) { return object->via.f64; }
int msgpack_object_boolean(msgpack_object* object) { return object->via.boolean; }
char* msgpack_object_string(msgpack_object* object)
{
  char* string = calloc(object->via.str.size, sizeof(char));
  memcpy(string, object->via.str.ptr, object->via.str.size);
  return string;
}

// returns a object array values
int msgpack_object_array_size(msgpack_object* object) { return object->via.array.size; }
msgpack_object* msgpack_object_array_element(msgpack_object* object, int index)
{
  return &(object->via.array.ptr[index]);
}

// frees an unpacked
void msgpack_unpacked_free(msgpack_unpacked* unpacked)
{
  msgpack_unpacked_destroy(unpacked);
  free(unpacked);
}

")

;; msgpack-sbuffer pointers definitions
(define-foreign-type msgpack-sbuffer "msgpack_sbuffer")
(define-foreign-type msgpack-sbuffer* (c-pointer msgpack-sbuffer))

;; msgpack-packer pointers definitions
(define-foreign-type msgpack-packer "msgpack_packer")
(define-foreign-type msgpack-packer* (c-pointer msgpack-packer))

;; msgpack-unpacked pointers definitions
(define-foreign-type msgpack-unpacked "msgpack_unpacked")
(define-foreign-type msgpack-unpacked* (c-pointer msgpack-unpacked))

;; msgpack-object pointers definitions
(define-foreign-type msgpack-object "msgpack_object")
(define-foreign-type msgpack-object* (c-pointer msgpack-object))

;; creates and initialize a sbuffer
(define msgpack-sbuffer-new (foreign-lambda msgpack-sbuffer* "msgpack_sbuffer_new"))
(define msgpack-sbuffer-init (foreign-lambda void "msgpack_sbuffer_init" msgpack-sbuffer*))
(define msgpack-sbuffer-free (foreign-lambda void "msgpack_sbuffer_free" msgpack-sbuffer*))

;; returns a sbuffer data
(define msgpack-sbuffer-data (foreign-lambda c-string* "msgpack_sbuffer_data" msgpack-sbuffer*))

;; creates a packer
(define msgpack-packer-new (foreign-lambda msgpack-packer* "msgpack_packer_new_wrapped" msgpack-sbuffer*))
(define msgpack-packer-free (foreign-lambda void "msgpack_packer_free" msgpack-packer*))

;; packing
(define msgpack-pack-int (foreign-lambda int "msgpack_pack_int" msgpack-packer* int))
(define msgpack-pack-double (foreign-lambda int "msgpack_pack_double" msgpack-packer* double))
(define msgpack-pack-true (foreign-lambda int "msgpack_pack_true" msgpack-packer*))
(define msgpack-pack-false (foreign-lambda int "msgpack_pack_false" msgpack-packer*))
(define msgpack-pack-array (foreign-lambda int "msgpack_pack_array" msgpack-packer* int))
(define msgpack-pack-str (foreign-lambda int "msgpack_pack_str" msgpack-packer* int))
(define msgpack-pack-str-body (foreign-lambda int "msgpack_pack_str_body" msgpack-packer* c-string int))

;; creates an unpacked
(define msgpack-unpacked-new (foreign-lambda msgpack-unpacked* "msgpack_unpacked_new"))
(define msgpack-unpacked-init (foreign-lambda void "msgpack_unpacked_init" msgpack-unpacked*))
(define msgpack-unpacked-free (foreign-lambda void "msgpack_unpacked_free" msgpack-unpacked*))

;; unpacks the next object
(define msgpack-unpack-next
  (foreign-lambda int "msgpack_unpack_next"
    msgpack-unpacked* u8vector int u64vector))

;; returns an unpacked current object
(define msgpack-unpacked-object (foreign-lambda msgpack-object* "msgpack_unpacked_object" msgpack-unpacked*))

;; returns an object type
(define msgpack-object-type (foreign-lambda int "msgpack_object_type_getter" msgpack-object*))

;; returns an object value
(define msgpack-object-int (foreign-lambda int "msgpack_object_int" msgpack-object*))
(define msgpack-object-double (foreign-lambda double "msgpack_object_double" msgpack-object*))
(define msgpack-object-boolean (foreign-lambda int "msgpack_object_boolean" msgpack-object*))
(define msgpack-object-string (foreign-lambda c-string* "msgpack_object_string" msgpack-object*))

;; returns an object array values
(define msgpack-object-array-size (foreign-lambda int "msgpack_object_array_size" msgpack-object*))
(define msgpack-object-array-element
  (foreign-lambda msgpack-object* "msgpack_object_array_element"
    msgpack-object* int))
