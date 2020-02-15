(declare (unit cbor))

(foreign-declare "

#include <stdio.h>
#include <cbor.h>

// structure for the serialized data
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
  if (size == 0)
  {
    return NULL;
  }

  struct cbor_serialize_data* data = malloc(sizeof(struct cbor_serialize_data));
  if (data == NULL)
  {
    free(buffer);
    return NULL;
  }

  data->buffer = buffer;
  data->size = size;

  return data;
}

// returns the size of a serialized data
int cbor_serialize_data_size(struct cbor_serialize_data* data)
{
  return data->size;
}

// copies a serialized data
void cbor_serialize_data_copy(struct cbor_serialize_data* data, char* u8vector)
{
  memcpy(u8vector, data->buffer, data->size);
}

// frees a serialized data
void cbor_serialize_data_free(struct cbor_serialize_data* data)
{
  free(data->buffer);
  free(data);
}

// deserializes an item
struct cbor_item_t* cbor_load_wrapped(char* data, int size)
{
  struct cbor_load_result result;
  return cbor_load(data, size, &result);
}

")

;; cbor-item pointers definitions
(define-foreign-type cbor-item-t "cbor_item_t")
(define-foreign-type cbor-item-t* (c-pointer cbor-item-t))

;; cbor-serialize-data pointers definitions
(define-foreign-type cbor-serialize-data "struct cbor_serialize_data")
(define-foreign-type cbor-serialize-data* (c-pointer cbor-serialize-data))

;; types
(define cbor-typeof (foreign-lambda int "cbor_typeof" cbor-item-t*))
(define cbor-type-positive-integer (foreign-value "0" int))
(define cbor-type-negative-integer (foreign-value "1" int))
(define cbor-type-byte-string (foreign-value "2" int))
(define cbor-type-string (foreign-value "3" int))
(define cbor-type-array (foreign-value "4" int))
(define cbor-type-decimals-and-ctrl (foreign-value "7" int))

;; booleans and null
(define cbor-build-bool (foreign-lambda cbor-item-t* "cbor_build_bool" bool))
(define cbor-ctrl-is-bool (foreign-lambda bool "cbor_ctrl_is_bool" cbor-item-t*))
(define cbor-new-null (foreign-lambda cbor-item-t* "cbor_new_null"))
(define cbor-is-null (foreign-lambda bool "cbor_is_null" cbor-item-t*))

;; integers
(define cbor-build-uint32 (foreign-lambda cbor-item-t* "cbor_build_uint32" int))
(define cbor-mark-negint (foreign-lambda void "cbor_mark_negint" cbor-item-t*))
(define cbor-get-uint32 (foreign-lambda int "cbor_get_uint32" cbor-item-t*))

;; floats
(define cbor-build-float8 (foreign-lambda cbor-item-t* "cbor_build_float8" double))
(define cbor-float-get-float (foreign-lambda double "cbor_float_get_float" cbor-item-t*))

;; strings
(define cbor-build-bytestring (foreign-lambda cbor-item-t* "cbor_build_bytestring" c-string int))
(define cbor-bytestring-handle (foreign-lambda c-string "cbor_bytestring_handle" cbor-item-t*))

;; arrays
(define cbor-new-indefinite-array (foreign-lambda cbor-item-t* "cbor_new_indefinite_array"))
(define cbor-array-is-indefinite (foreign-lambda bool "cbor_array_is_indefinite" cbor-item-t*))
(define cbor-array-size (foreign-lambda int "cbor_array_size" cbor-item-t*))
(define cbor-array-push (foreign-lambda bool "cbor_array_push" cbor-item-t* cbor-item-t*))
(define cbor-array-get (foreign-lambda cbor-item-t* "cbor_array_get" cbor-item-t* int))

;; serializes an item
(define cbor-serialize-alloc (foreign-lambda cbor-serialize-data* "cbor_serialize_alloc_wrapped" cbor-item-t*))
(define cbor-serialize-data-size (foreign-lambda int "cbor_serialize_data_size" cbor-serialize-data*))
(define cbor-serialize-data-copy (foreign-lambda void "cbor_serialize_data_copy" cbor-serialize-data* u8vector))
(define cbor-serialize-data-free (foreign-lambda void "cbor_serialize_data_free" cbor-serialize-data*))

;; deserializes an item
(define cbor-load (foreign-lambda cbor-item-t* "cbor_load_wrapped" u8vector int))

;; cleanup
(define cbor-intermediate-decref (foreign-lambda void "cbor_intermediate_decref" cbor-item-t*))
