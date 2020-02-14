(declare (unit cbor))

(foreign-declare "

#include <stdio.h>
#include <cbor.h>

// structure for the serialize data
struct cbor_serialize_data
{
  char* buffer;
  int size;
};

// serializes an item
struct cbor_serialize_data* cbor_serialize_alloc_wrapped(cbor_item_t* item)
{
  unsigned char* buffer = NULL;
  size_t buffer_size = 0;

  int size = cbor_serialize_alloc(item, &buffer, &buffer_size);

  struct cbor_serialize_data* data = malloc(sizeof(struct cbor_serialize_data));
  data->buffer = buffer;
  data->size = size;

  return data;
}

// returns the size of a serialized item
int cbor_serialize_data_size(struct cbor_serialize_data* data)
{
  return data->size;
}

// copies a serialized item
void cbor_serialize_data_copy(struct cbor_serialize_data* data, char* u8vector)
{
  memcpy(u8vector, data->buffer, data->size);
}

// frees a serialized item
void cbor_serialize_data_free(struct cbor_serialize_data* data)
{
  free(data->buffer);
}

")

;; cbor-item pointers definitions
(define-foreign-type cbor-item-t "cbor_item_t")
(define-foreign-type cbor-item-t* (c-pointer cbor-item-t))

;; cbor-serialize-data pointers definitions
(define-foreign-type cbor-serialize-data "struct cbor_serialize_data")
(define-foreign-type cbor-serialize-data* (c-pointer cbor-serialize-data))

;; booleans and null
(define cbor-build-bool (foreign-lambda cbor-item-t* "cbor_build_bool" bool))
(define cbor-build-ctrl (foreign-lambda cbor-item-t* "cbor_build_ctrl" int))
(define cbor-ctrl-value (foreign-lambda int "cbor_ctrl_value" cbor-item-t*))

;; integers
(define cbor-build-uint64 (foreign-lambda cbor-item-t* "cbor_build_uint64" int))
(define cbor-mark-negint (foreign-lambda void "cbor_mark_negint" cbor-item-t*))
(define cbor-get-uint64 (foreign-lambda int "cbor_get_uint64" cbor-item-t*))

;; floats
(define cbor-build-float8 (foreign-lambda cbor-item-t* "cbor_build_float8" double))
(define cbor-float-get-float (foreign-lambda double "cbor_float_get_float" cbor-item-t*))

;; strings
(define cbor-build-bytestring (foreign-lambda cbor-item-t* "cbor_build_bytestring" c-string int))
(define cbor-bytestring-handle (foreign-lambda c-string* "cbor_bytestring_handle" cbor-item-t*))

;; arrays
(define cbor-new-definite-array (foreign-lambda cbor-item-t* "cbor_new_definite_array" int))
(define cbor-array-push (foreign-lambda bool "cbor_array_push" cbor-item-t* cbor-item-t*))

;; serializes an item
(define cbor-serialize-alloc (foreign-lambda cbor-serialize-data* "cbor_serialize_alloc_wrapped" cbor-item-t*))
(define cbor-serialize-data-size (foreign-lambda int "cbor_serialize_data_size" cbor-serialize-data*))
(define cbor-serialize-data-copy (foreign-lambda void "cbor_serialize_data_copy" cbor-serialize-data* u8vector))
(define cbor-serialize-data-free (foreign-lambda void "cbor_serialize_data_free" cbor-serialize-data*))

;; cleanup
(define cbor-intermediate-decref (foreign-lambda void "cbor_intermediate_decref" cbor-item-t*))
